package org.specs2
package reporter

import specification._
import specification.process._
import core._
import scalaz.concurrent.Task
import control._
import Actions._
import scalaz.{Writer => _, _}, Scalaz._
import Task._
import scalaz.stream.Process
import data._
import Processes._
import Fold._
import Printer._
import SpecStructure._

/**
 * A reporter is responsible to select printers based on the arguments
 * execute the specification and pass it to each printer for printing
 *
 * It is also responsible for saving the specification state at the end of the run
 */
trait Reporter {

  def report(env: Env, printers: List[Printer]): SpecStructure => Action[Unit] = { spec =>
    val env1 = env.copy(arguments = env.arguments.overrideWith(spec.arguments))
    val executing = spec |> Filter.filter(env1) |> Executor.execute(env1)
    val folds = printers.map(_.fold(env1, spec)) :+ statsStoreFold(env1, spec)
    Actions.fromTask(runFolds(executing.contents, folds))
  }

  /**
   * Use a Fold to store the stats of each example + the stats of the specification
   */
  def statsStoreFold(env: Env, spec: SpecStructure) = new Fold[Fragment] {
    type S = Stats

    lazy val sink: Process.Sink[Task, (Fragment, Stats)] =
      Process.constant {  case (fragment: Fragment, stats: Stats) =>
        env.statisticsRepository.storeResult(spec.specClassName, fragment.description, fragment.executionResult).toTask
      }

    def fold = Statistics.fold
    def init = Stats()

    def last(stats: Stats) =
      env.statisticsRepository.storeStatistics(spec.specClassName, stats).toTask
  }

}

object Reporter extends Reporter

