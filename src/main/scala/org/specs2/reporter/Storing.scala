package org.specs2
package reporter

import org.specs2.internal.scalaz._
import Scalaz._
import specification._
import NestedBlocks._
import collection.Iterablex._
import main.{Execute, Arguments}

/**
* This trait stores the results of an executed specification
* It also updates the statistics of:
*
*  * SpecStart fragments from SpecEnd fragments
*/
private[specs2]
trait Storing {
  /** @return a function storing ExecutedFragments */
  def store(implicit args: Arguments): Seq[ExecutedFragment] => Seq[ExecutedFragment]

}

private[specs2]
trait DefaultStoring extends Storing with Statistics with WithDefaultStatisticsRepository {

  def store(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => {
    if (args.store.reset) repository.resetStatistics
    associateStartEnd(statisticsTotals(fragments) , updateStatsOnSpecStart) map (_.value) map storeStats
  }

  private def statisticsTotals(fragments: Seq[ExecutedFragment]) = {
    val totals = fragments zip fragments.reduceWith(StatisticsReducer).totals
    totals map (setStatsOnSpecEndFragments andThen executedFragmentsToSpecBlock)
  }

  /**
   * set the statistics on SpecEndFragments after they've been computed by the StatisticsReducer
   * Those statistics are updated from previously executed statistics to calculate trends
   */
  private def setStatsOnSpecEndFragments = (fs: (ExecutedFragment, Stats)) => fs match {
    case (ExecutedSpecEnd(n, l, s), stats) => ExecutedSpecEnd(n, l, stats.updateFrom(repository.getStatistics(n.specName)))
    case (other, s)                        => other
  }

  /**
   *  "associate" function to set up the statistics on the SpecStart fragments after they've been computed and set on SpecEnd fragments.
   */
  private def updateStatsOnSpecStart = (start: ExecutedFragment, end: ExecutedFragment) => {
    (start, end) match {
      case (ExecutedSpecStart(ns, ss, ls), ExecutedSpecEnd(ne, se, le)) => (ExecutedSpecStart(ns, se, le), ExecutedSpecEnd(ne, se, le))
      case other => (start, end)
    }
  }

  /**
   * store the statistics:
   * * for SpecEnd -> put the stats in the repository
   * * for a SpecStart that's a link -> read the status of the previous execution 
   */
  protected def storeStats = (f: ExecutedFragment) => {
    f match {
      case ExecutedSpecStart(start @ SpecStart(_,_,_,true), loc, st) =>
        ExecutedSpecStart(start, loc, repository.getStatistics(start.specName).getOrElse(st))

      case ExecutedSpecEnd(end @ SpecEnd(_), loc, st) => repository.storeStatistics(end.specName, st); f
      case other                                      => other
    }
  }

}
