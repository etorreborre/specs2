package org.specs2
package reporter

import scalaz._
import Scalaz._
import specification._
import NestedBlocks._
import collection.Iterablex._
import main.Arguments

/**
* This trait stores the results of an executed specification
* It also updates the statistics of:
*
*  - SpecStart fragments from SpecEnd fragments
*/
private[specs2]
trait Storing {
  /** @return a function storing ExecutedFragments */
  def store(implicit args: Arguments): ExecutingSpecification => ExecutingSpecification

}

private[specs2]
trait DefaultStoring extends Storing with Statistics with WithDefaultStatisticsRepository {

  def store(implicit args: Arguments): ExecutingSpecification => ExecutingSpecification = (spec: ExecutingSpecification) => {
    if (args.store.reset) repository.resetStatistics
    val fragmentsWithSpecStartUpdatedWithStatistics =
      associateStartEnd(statisticsTotals(spec.execute.fragments), updateStatsOnSpecStart) |> storeStatistics

    spec.copy(fs = fragmentsWithSpecStartUpdatedWithStatistics.map(FinishedExecutingFragment))
  }

  private def statisticsTotals(fragments: Seq[ExecutedFragment])(implicit args: Arguments) = {
    val totals = fragments zip fragments.reduceWith(ExecutedStatisticsReducer).totals
    totals map (setStatsOnSpecEndFragments andThen executedFragmentsToSpecBlock)
  }

  /**
   * set the statistics on SpecEndFragments after they've been computed by the StatisticsReducer
   * Those statistics are updated from previously executed statistics to calculate trends
   */
  def setStatsOnSpecEndFragments(implicit args: Arguments) = (fs: (ExecutedFragment, Stats)) => fs match {
    case (ExecutedSpecEnd(n, l, s), stats) if !args.store.never => ExecutedSpecEnd(n, l, stats.updateFrom(repository.getStatistics(n.specName)))
    case (other, s)                                             => other
  }

  /**
   *  "associate" function to set up the statistics on the SpecStart fragments after they've been computed and set on SpecEnd fragments.
   */
  private def updateStatsOnSpecStart = (start: ExecutedFragment, end: ExecutedFragment) => {
    (start, end) match {
      case (ExecutedSpecStart(ns, ss, ls), ExecutedSpecEnd(ne, se, le)) => (ExecutedSpecStart(ns, se, le), ExecutedSpecEnd(ne.seeOnlyLinkIs(ns.isSeeOnlyLink), se, le))
      case other                                                        => (start, end)
    }
  }

  private def storeStatistics(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => {
    if (args.store.never) fragments
    else {
      val fn = SpecsArguments.foldAll(fragments).fragmentAndSpecNames
      val results = fn collect { case (r @ ExecutedResult(_, _, _, _, _), name) => (name, r) } groupBy (_._1)
      results map storeResults
      // toList is called to "force" the view
      // the result of storeStats is returned because SpecStarts might have been updated from the repository
      fn.toList map storeStats
    }
  }
  /**
   * store the statistics:
   * - for SpecEnd -> put the stats in the repository
   * - for a SpecStart that's a link -> read the status of the previous execution
   */
  protected def storeStats = (fn: (ExecutedFragment, SpecName)) => {
    fn match {
      case (ExecutedSpecStart(start: SpecStart, loc, st), _) if start.isSeeOnlyLink =>
        ExecutedSpecStart(start, loc, repository.getStatistics(start.specName).getOrElse(st))
      // if the specification is see-only don't store the stats
      // otherwise the index page will think that the last execution was successful
      case (f @ ExecutedSpecEnd(end: SpecEnd, loc, st), _) if !end.isSeeOnlyLink => repository.storeStatistics(end.specName, st); f
      case (other, name)                                                         => other
    }
  }
  /**
   * store the results by spec name
   */
  protected def storeResults = (r: (SpecName, Seq[(SpecName, ExecutedResult)])) => {
    val (name, results) = r
    repository.storeResults(name, results.map(_._2))
  }
}
