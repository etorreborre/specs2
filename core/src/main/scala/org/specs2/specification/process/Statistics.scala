package org.specs2
package specification
package process

import control._
import specification.core._
import scalaz.concurrent.Task
import scalaz.stream._
import foldm._, FoldM._
import stream.FoldableProcessM._
import stream.FoldProcessM._

/**
 * Compute the statistics for executed fragments
 */
trait Statistics {

  def statsProcess: Process1[Fragment, Stats] =
    process1.reduceMap { fragment =>
      fragment.execution.executedResult.map(Stats.apply).getOrElse(Stats.empty)
    }

  def defaultStats(fragment: Fragment) =
    if (Fragment.isExample(fragment)) Stats(examples = 1)
    else                              Stats.empty

  /**
   * load the previous statistics if necessary
   */
  def readStats(spec: SpecStructure, env: Env): SpecStructure =
    if (env.arguments.wasIsDefined) spec.flatMap(readStats(spec.specClassName, env))
    else                            spec

  /**
   * read the stats for one Fragment
   */
  def readStats(className: String, env: Env): Fragment => Process[Task, Fragment] = { f: Fragment =>
    Process.eval(env.statisticsRepository.previousResult(className, f.description).map(r => f.setPreviousResult(r)).toTask(env.systemLogger))
  }

  def fold: FoldState[Fragment, Stats] =
    FoldM.fromMonoidMap { fragment: Fragment =>
      fragment.execution.executedResult.fold(defaultStats(fragment)) { result =>
        defaultStats(fragment).withResult(result).copy(timer = fragment.execution.executionTime)
      }
    }
}

object Statistics extends Statistics {
  /** compute the statistics as a Fold */
  def statisticsFold: FoldM[Fragment, Task, Stats] { type S = Stats } =
    fold.into[Task]

  def runStats(spec: SpecStructure): Stats =
    statisticsFold.run[ProcessTask](spec.contents).run
}
