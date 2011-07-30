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
  def store(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => {
    val totals = fragments zip fragments.reduceWith(StatisticsReducer).totals
    associateStartEnd(totals map (setStats andThen executedFragmentsToSpecBlock), updateStatsOnSpecStart) map (_.value)
  }

  private def setStats = (fs: (ExecutedFragment, Stats)) => fs match {
    case (ExecutedSpecStart(n, l, s), stats) => ExecutedSpecStart(n, l, stats)
    case (ExecutedSpecEnd(n, l, s), stats)   => ExecutedSpecEnd(n, l, stats)
    case (other, s)                          => other
  }


  private def updateStatsOnSpecStart = (start: ExecutedFragment, end: ExecutedFragment) => {
    (start, end) match {
      case (ExecutedSpecStart(ns, ss, ls), ExecutedSpecEnd(ne, se, le)) => (ExecutedSpecStart(ns, se, le), ExecutedSpecEnd(ne, se, le))
      case other => (start, end)
    }
  }


}
