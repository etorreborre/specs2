package org.specs2
package reporter

import specification._
import specification.process._
import core._
import control._
import eff.all._
import origami._, Folds._
import producer._
import scalaz._, Scalaz._
import Statistics._

/**
 * A reporter is responsible for
 *  - selecting printers based on the command-line arguments
 *  - executing the specification
 *  - passing it to each printer for printing
 *
 * It is also responsible for saving the specification state at the end of the run
 */
trait Reporter {

  def prepare(env: Env, printers: List[Printer]): List[SpecStructure] => Action[Unit] = { specs =>
    printers.traverseU(_.prepare(env, specs)).void
  }

  def finalize(env: Env, printers: List[Printer]): List[SpecStructure] => Action[Unit] = { specs =>
    printers.traverseU(_.finalize(env, specs)).void
  }

  /**
   * report 1 spec structure with the given printers
   * first find and sort the referenced specifications and report them
   */
  def report(env: Env, printers: List[Printer]): SpecStructure => Action[Stats] = { spec =>
    val env1 = env.setArguments(env.arguments.overrideWith(spec.arguments))
    val executing = readStats(spec, env1) |> env1.selector.select(env1) |> env1.executor.execute(env1)
    
    val contents: AsyncStream[Fragment] =
      // evaluate all fragments before reporting if required
      if (env.arguments.execute.asap) producers.emitEff(executing.contents.runList)
      else                            executing.contents

    val sinks = (printers.map(_.sink(env1, spec)) :+ statsStoreSink(env1, spec)).suml
    val reportFold = Statistics.statisticsFold <* sinks

    contents.fold(reportFold)
  }

  /**
   * Use a Fold to store the stats of each example + the stats of the specification
   */
  def statsStoreSink(env: Env, spec: SpecStructure): AsyncSink[Fragment] = {
    val neverStore = env.arguments.store.never
    val resetStore = env.arguments.store.reset

    lazy val sink: AsyncSink[Fragment] =
      Folds.sink[ActionStack, Fragment] { fragment: Fragment =>
        if (neverStore) Actions.unit
        else            env.statisticsRepository.storeResult(spec.specClassName, fragment.description, fragment.executionResult)
      }

    val prepare: Action[Unit] =
      if (resetStore) env.statisticsRepository.resetStatistics
      else            Actions.unit

    val last = (stats: Stats) =>
      if (neverStore) Actions.unit
      else            env.statisticsRepository.storeStatistics(spec.specClassName, stats)

    (Statistics.fold.into[ActionStack] <* fromStart(prepare) <* sink).mapFlatten(last)
  }

}

object Reporter extends Reporter

