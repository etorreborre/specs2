package org.specs2
package reporter

import org.specs2.internal.scalaz._
import Scalaz._
import main.Arguments
import specification._
import NestedBlocks._
import collection.Iterablex._
/**
 * This trait defines a very generic way to store the results of an executed specification
 */
private[specs2]
trait Storing {

  /** @return a function storing ExecutedFragments */
  def store(implicit args: Arguments): Seq[ExecutedFragment] => Seq[ExecutedFragment]
}

private[specs2]
trait DefaultStoring extends Storing with Statistics {

  protected lazy val repository: StatisticsRepository = DefaultStatisticsRepository

  def store(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => {
    val totals = fragments zip fragments.reduceWith(StatisticsReducer).totals
    associateStartEnd(totals map (setStats andThen executedFragmentsToSpecBlock), updateStatsOnSpecStart) map (_.value) map storeStats
  }

  protected def setStats = (fs: (ExecutedFragment, Stats)) => fs match {
    case (ExecutedSpecEnd(n, l, s), stats) => ExecutedSpecEnd(n, l, (repository.getStatistics(n.specName) map stats.updatedFrom).getOrElse(stats))
    case (other, s)                        => other
  }


  protected def updateStatsOnSpecStart = (start: ExecutedFragment, end: ExecutedFragment) => {
    (start, end) match {
      case (ExecutedSpecStart(ns, ss, ls), ExecutedSpecEnd(ne, se, le)) => (ExecutedSpecStart(ns, se, le), ExecutedSpecEnd(ne, se, le))
      case other => (start, end)
    }
  }

  protected def storeStats = (f: ExecutedFragment) => {
    f match {
      case ExecutedSpecStart(start @ SpecStart(_,_,_,true), loc, st) =>
        ExecutedSpecStart(start, loc, repository.getStatistics(start.specName).getOrElse(st))
      case ExecutedSpecEnd(end @ SpecEnd(_), loc, st)                =>
        repository.storeStatistics(end.specName, st); f
      case other => other
    }
  }

}
