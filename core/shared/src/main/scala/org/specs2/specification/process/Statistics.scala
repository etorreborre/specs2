package org.specs2
package specification
package process

import control._
import specification.core._

import control._
import origami._
import producer._

/**
 * Compute the statistics for executed fragments
 */
trait Statistics {

  def statsProcess: AsyncTransducer[Fragment, Stats] =
    transducers.reduceMap { fragment =>
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
  def readStats(className: String, env: Env): Fragment => AsyncStream[Fragment] = { f: Fragment =>
    producers.eval(env.statisticsRepository.previousResult(className, f.description).map(r => f.setPreviousResult(r)))
  }

  def fold: FoldState[Fragment, Stats] =
    Folds.fromMonoidMap { fragment: Fragment =>
      fragment.execution.executedResult.fold(defaultStats(fragment)) { result =>
        defaultStats(fragment).withResult(result).copy(timer = fragment.execution.executionTime)
      }
    }
}

object Statistics extends Statistics {
  /** compute the statistics as a Fold */
  def statisticsFold: Fold[ActionStack, Fragment, Stats] { type S = Stats } =
    fold.into[ActionStack]

  def runStats(spec: SpecStructure): Stats =
    spec.contents.fold(statisticsFold).runOption.getOrElse(Stats.empty)
}
