package org.specs2
package reporter

import foldm._, FoldId._, FoldM._
import stream.FoldProcessM._
import specification._
import specification.process._
import core._
import scalaz.concurrent.Task
import control._
import scalaz.{Writer => _, _}, Scalaz._
import scalaz.stream._
import data._
import Fold._
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
  def report(env: Env, printers: List[Printer]): SpecStructure => Action[Unit] = { spec =>
    val env1 = env.setArguments(env.arguments.overrideWith(spec.arguments))
    val executing = readStats(spec, env1) |> env1.selector.select(env1) |> env1.executor.execute(env1)
    
    val contents =
      // evaluate all fragments before reporting if required
      if (env.arguments.execute.asap) Process.eval(executing.contents.runLog).flatMap(Process.emitAll)
      else                            executing.contents

    val sinks = printers.map(_.sink(env1, spec)) :+ statsStoreSink(env1, spec)
    Actions.fromTask(runSinks(contents, sinks))
  }

  /**
   * Use a Fold to store the stats of each example + the stats of the specification
   */
  def statsStoreSink(env: Env, spec: SpecStructure): SinkTask[Fragment] = {
    val neverStore = env.arguments.store.never
    val resetStore = env.arguments.store.reset

    lazy val sink: Sink[Task, Fragment] =
      channel.lift {  case fragment =>
        if (neverStore) Task.delay(())
        else            env.statisticsRepository.storeResult(spec.specClassName, fragment.description, fragment.executionResult).toTask
      }

    val prepare: Task[Unit] =
      if (resetStore) env.statisticsRepository.resetStatistics.toTask
      else            Task.now(())

    val last = (stats: Stats) =>
      if (neverStore) Task.now(())
      else            env.statisticsRepository.storeStatistics(spec.specClassName, stats).toTask

    (Statistics.fold.into[Task] <* fromStart(prepare) <* fromSink(sink)).mapFlatten(last)
  }

}

object Reporter extends Reporter

