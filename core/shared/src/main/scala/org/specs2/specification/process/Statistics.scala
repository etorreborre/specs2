package org.specs2
package specification
package process

import control._
import Actions._
import specification.core._
import control._
import org.specs2.concurrent.ExecutionEnv
import origami._
import producer._
import ExecuteActions._

/**
 * Compute the statistics for executed fragments
 */
trait Statistics {

  def statsProcess: AsyncTransducer[Fragment, Stats] =
    transducers.reduceMapEval[ActionStack, Fragment, Stats](_.executionResult.map(Stats.apply))

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
  def readStats(className: String, env: Env): Fragment => AsyncStream[Fragment] = { f: Fragment =>
    producers.eval(env.statisticsRepository.previousResult(className, f.description).map(r => f.setPreviousResult(r)))
  }

  def fold: AsyncFold[Fragment, Stats] { type S = Stats } =
    Folds.fromMonoidMapEval { fragment: Fragment =>
      if (fragment.isExecutable) {
        fragment.executedResult.map { case ExecutedResult(result, timer) =>
          defaultStats(fragment).withResult(result).copy(timer = timer)
        }
      } else ok(Stats.empty)
    }
}

object Statistics extends Statistics {
  def runStats(spec: SpecStructure)(ee: ExecutionEnv): Stats =
    spec.contents.fold(fold).runOption(ee).getOrElse(Stats.empty)
}
