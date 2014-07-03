package org.specs2
package specification
package process

import control._
import specification.core._
import scalaz.concurrent.Task
import scalaz.stream.{Process, process1}
import Process.Process1
import scalaz.syntax.monoid._
import scalaz.std.anyVal._

/**
 * Compute the statistics for executed fragments
 */
trait Statistics {

  def statsProcess: Process1[Fragment, Stats] =
    process1.reduceMap { fragment =>
      fragment.execution.executedResult.map(Stats.apply).getOrElse(Stats())
    }

  def fold = (fragment: Fragment, stats: Stats) =>
    stats |+| fragment.execution.executedResult.fold(defaultStats(fragment)) { result =>
      Stats(result).copy(timer = fragment.execution.executionTime)
    }

  def defaultStats(fragment: Fragment) =
    if (Fragment.isExample(fragment)) Stats(examples = 1)
    else                              Stats()

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

object Statistics extends Statistics
