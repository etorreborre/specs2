package org.specs2
package specification
package process

import control._
import org.specs2.data.Fold
import specification.core._
import scalaz.concurrent.Task
import scalaz.stream._
import scalaz.syntax.monoid._

/**
 * Compute the statistics for executed fragments
 */
trait Statistics {

  def statsProcess: Process1[Fragment, Stats] =
    process1.reduceMap { fragment =>
      fragment.execution.executedResult.map(Stats.apply).getOrElse(Stats.empty)
    }

  def fold = (fragment: Fragment, stats: Stats) =>
    stats |+| fragment.execution.executedResult.fold(defaultStats(fragment)) { result =>
      defaultStats(fragment).withResult(result).copy(timer = fragment.execution.executionTime)
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
    Process.eval(env.statisticsRepository.previousResult(className, f.description).map(r => f.setPreviousResult(r)).toTask)
  }


}

object Statistics extends Statistics {
  /** compute the statistics as a Fold */
  def statisticsFold = new Fold[Fragment] {
    type S = Stats

    def prepare: Task[Unit] = Task.now(())

    lazy val sink: Sink[Task, (Fragment, Stats)] = Fold.unitSink

    def fold = Statistics.fold
    def init = Stats.empty

    def last(stats: Stats) = Task.now(())
  }

  def runStats(spec: SpecStructure): Stats =
    Fold.runFoldLast(spec.contents, Statistics.statisticsFold).run
}
