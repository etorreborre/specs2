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
 * A reporter is responsible for
 *  - selecting printers based on the command-line arguments
 *  - executing the specification
 *  - passing it to each printer for printing
 *
 * It is also responsible for saving the specification state at the end of the run
 */

trait Reporter {

  def prepare(specs: List[SpecStructure]): Action[Unit]

  def finalize(specs: List[SpecStructure] ):  Action[Unit]

  def report(spec: SpecStructure): Action[Stats]

}

/**
 * Default implementation of a Reporter using specs2 Printers
 */
case class DefaultReporter(arguments: Arguments, env: Env, printers: List[Printer]) extends Reporter {

  def prepare(specs: List[SpecStructure]): Action[Unit] =
    printers.traverse(_.prepare(specs)).void

  def finalize(specs: List[SpecStructure] ): Action[Unit] =
    printers.traverse(_.finalize(specs)).void

  /**
   * report 1 spec structure with the given printers
   * first find and sort the referenced specifications and report them
   */
  def report(spec: SpecStructure): Action[Stats] = {
    val env1 = env.setArguments(env.arguments.overrideWith(spec.arguments))
    val executing = readStats(spec, env1) |> env1.selector.select(env1) |> env1.executor.execute(env1)

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
