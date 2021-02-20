package org.specs2
package reporter

import fp.{given, *}, syntax.{given, *}
import control.*
import producer.*
import origami.*, Folds.*
import specification.*
import specification.process.*
import core.*
import control.*
import main.Arguments

/**
 * A reporter executes a list of specification and return statistics
 * representing the execution results.
 *
 * A full list of specification is passed instead of just one specification because
 * it gives the possibility to the reporter to prepare and finalize the reporting
 * according to the needs of each Printer (see the Printer doc)
 */

trait Reporter:

  def report(specs: List[SpecStructure]): Action[Stats]

  /** convenience method to execute several specifications as varargs */
  def report(specs: SpecStructure*): Action[Stats] =
    report(specs.toList)


/**
 * Default implementation of a Reporter using specs2 Printers
 */
case class DefaultReporter(statistics: Statistics, statisticsRepository: StatisticsRepository, selector: Selector, executor: Executor, printers: List[Printer], env: Env) extends Reporter:

  def report(specs: List[SpecStructure]): Action[Stats] = for
    _     <- prepare(specs)
    stats <- specs.traverse(reportOne)
    _     <- finalize(specs)
  yield stats.suml

  def prepare(specs: List[SpecStructure]): Action[Unit] =
    printers.traverse(_.prepare(specs)).void

  def finalize(specs: List[SpecStructure] ): Action[Unit] =
    printers.traverse(_.finalize(specs)).void

  /**
   * report 1 spec structure with the given printers
   * first find and sort the referenced specifications and report them
   */
  def reportOne(spec: SpecStructure): Action[Stats] =
    val executing =
      statistics.readStats(spec) |>
      selector.select(spec.arguments) |>
      executor.execute(spec.arguments)

    val contents: AsyncStream[Fragment] =
      // evaluate all fragments before reporting if required
      if env.arguments.execute.asap then Producer.emitAction(executing.contents.runList)
      else                            executing.contents

    val sinks = (printers.map(_.sink(spec)) :+ statsStoreSink(spec)).sumAll
    val reportFold = sinks *> Statistics.fold

    contents.fold(reportFold)

  /**
   * Use a Fold to store the stats of each example + the stats of the specification
   */
  private def statsStoreSink(spec: SpecStructure): AsyncSink[Fragment] =
    val arguments = env.arguments.overrideWith(spec.arguments)
    val neverStore = arguments.store.never
    val resetStore = arguments.store.reset

    lazy val sink: AsyncSink[Fragment] =
      Folds.fromSink[Action, Fragment] { (fragment: Fragment) =>
        fragment.executionResult.flatMap { r =>
          statisticsRepository.storeResult(spec.specClassName, fragment.description, r).toAction
        }.unless(neverStore)
      }

    val prepare: Action[Unit] =
      when(resetStore)(statisticsRepository.resetStatistics).toAction

    val last = (stats: Stats) =>
      unless(neverStore)(statisticsRepository.storeStatistics(spec.specClassName, stats)).toAction

    (Statistics.fold <* fromStart(prepare) <* sink).mapFlatten(last)


object Reporter:

  def create(printers: List[Printer], env: Env): Reporter =
    val arguments = env.arguments
    val statistics = DefaultStatistics(arguments, env.statisticsRepository)
    val selector = arguments.select.selector.flatMap(Arguments.instance[Selector]).getOrElse(DefaultSelector(arguments))
    val executor = arguments.execute.executor.flatMap(Arguments.instance[Executor]).getOrElse(DefaultExecutor(env))
    DefaultReporter(statistics, env.statisticsRepository, selector, executor, printers, env)

  def createCustomInstance(customInstances: CustomInstances): Operation[Option[Reporter]] =
    customInstances.createCustomInstance[Reporter]("reporter", (m: String) => "a custom reporter can not be instantiated " + m, "no custom reporter defined, using the default one")
