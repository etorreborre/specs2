package org.specs2
package reporter

import fp._, syntax._
import control._
import producer._
import origami._, Folds._
import specification._
import specification.process._
import core._
import Statistics._
import main.Arguments

/**
 * A reporter executes a list of specification and return statistics
 * representing the execution results.
 *
 * A full list of specification is passed instead of just one specification because
 * it gives the possibility to the reporter to prepare and finalize the reporting
 * according to the needs of each Printer (see the Printer doc)
 */

trait Reporter {

  def report(specs: List[SpecStructure]): Action[Stats]

  /** convenience method to execute several specifications as varargs */
  def report(specs: SpecStructure*): Action[Stats] =
    report(specs.toList)

}

/**
 * Default implementation of a Reporter using specs2 Printers
 */
case class DefaultReporter(arguments: Arguments, env: Env, printers: List[Printer]) extends Reporter {

  def report(specs: List[SpecStructure]): Action[Stats] = for {
    _     <- prepare(specs)
    stats <- specs.traverse(reportOne)
    _     <- finalize(specs)
  } yield stats.suml

  def prepare(specs: List[SpecStructure]): Action[Unit] =
    printers.traverse(_.prepare(specs)).void

  def finalize(specs: List[SpecStructure] ): Action[Unit] =
    printers.traverse(_.finalize(specs)).void

  /**
   * report 1 spec structure with the given printers
   * first find and sort the referenced specifications and report them
   */
  def reportOne(spec: SpecStructure): Action[Stats] = {
    val env1 = env.setArguments(env.arguments.overrideWith(spec.arguments))
    val executing = readStats(spec, env1) |> env1.selector.select |> env1.executor.execute(env1)

    val contents: AsyncStream[Fragment] =
      // evaluate all fragments before reporting if required
      if (env.arguments.execute.asap) Producer.emitAction(executing.contents.runList)
      else                            executing.contents

    val sinks = (printers.map(_.sink(spec)) :+ statsStoreSink(env1, spec)).sumAll
    val reportFold = sinks *> Statistics.fold

    contents.fold(reportFold)
  }

  /**
   * Use a Fold to store the stats of each example + the stats of the specification
   */
  private def statsStoreSink(env1: Env, spec: SpecStructure): AsyncSink[Fragment] = {
    val neverStore = env1.arguments.store.never
    val resetStore = env1.arguments.store.reset

    lazy val sink: AsyncSink[Fragment] =
      Folds.fromSink[Action, Fragment] { fragment: Fragment =>
        if (neverStore)
          Action.unit
        else
          fragment.executionResult.flatMap { r =>
            env1.statisticsRepository.storeResult(spec.specClassName, fragment.description, r).toAction
          }
      }

    val prepare: Action[Unit] =
      if (resetStore) env1.statisticsRepository.resetStatistics.toAction
      else            Action.unit

    val last = (stats: Stats) =>
      if (neverStore) Action.unit
      else            env1.statisticsRepository.storeStatistics(spec.specClassName, stats).toAction

    (Statistics.fold <* fromStart(prepare) <* sink).mapFlatten(last)
  }

}
