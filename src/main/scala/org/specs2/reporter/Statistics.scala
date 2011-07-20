package org.specs2
package reporter

import org.specs2.internal.scalaz.{ Scalaz, Monoid, Reducer }
import Scalaz._
import collection.Iterablex._
import main.Arguments
import execute._
import org.specs2.execute.StandardResults
import time._
import specification._
import control.Exceptions._
import scala.xml.NodeSeq

/**
 * This trait computes the statistics of a Specification by mapping each ExecutedFragment
 * to a Stats object
 * 
 * Some stats objects embed their corresponding ExecutedSpecStart or ExecutedSpecEnd
 * fragment to be able to determine when the total Stats corresponding to all executed
 * specifications must be displayed
 * 
 * @see Stats.isEnd
 *
 */
private[specs2]
trait Statistics {
  import Stats._
  implicit def SpecsStatisticsMonoid  = new Monoid[SpecsStatistics] {
    def append(s1: SpecsStatistics, s2: =>SpecsStatistics): SpecsStatistics = {
      SpecsStatistics(s1.stats ++ s2.stats)
    }
    val zero = SpecsStatistics() 
  }

  def foldAll(fs: Seq[ExecutedFragment]) = fs.foldMap(StatisticsReducer.unit)
  
  object StatisticsReducer extends Reducer[ExecutedFragment, SpecsStatistics] {
    override def unit(f: ExecutedFragment): SpecsStatistics = f match { 
      case ExecutedResult(_, r, t, _) => {
        def resultStats(result: Result): Stats = {
          result match {
            case s @ Success(_)        => Stats(fragments = 1, expectations = s.expectationsNb, successes = 1)
            case Failure(_, _, _, _)   => Stats(fragments = 1, expectations = 1, failures = 1)
            case Error(_,_)            => Stats(fragments = 1, expectations = 1, errors = 1)
            case Pending(_)            => Stats(fragments = 1, expectations = 1, pending = 1)
            case Skipped(_, _)         => Stats(fragments = 1, expectations = 1, skipped = 1)
            case DecoratedResult(t, r) => resultStats(r)
          }
        }
        val current = resultStats(r)
        SpecsStatistics(current.copy(timer = t))
      }
      case start @ ExecutedSpecStart(name, args, _) => SpecsStatistics(Stats(start = Some(start)))
      case end @ ExecutedSpecEnd(_, _)              => SpecsStatistics(Stats(end = Some(end)))
      case ExecutedSee(link, _)                     => SpecsStatistics(stats(link))
      case ExecutedNoText(t, _)                     => SpecsStatistics(Stats(timer = t))
      case _                                        => SpecsStatistics(Stats())
    }
  }

  /** @return the statistics of a specification designated by a link if this can be read from the html report */
  def stats(link: HtmlLink) = Stats()

  /**
   * The SpecsStatistics class stores the result of a specification execution, with the
   * a list of 'current' stats for each fragment execution and the total statistics 
   * for the whole specification
   */
  case class SpecsStatistics(stats: List[Stats] = Nil) {
    private implicit val statsMonoid = Stats.StatsMonoid
    
    /** @return the list of all current stats, with the total on each line */
    def totals: List[Stats] = {
      import NestedBlocks._

      def toBlock(s: Stats) = s match {
        case Stats(_,_,_,_,_,_,_,_,Some(_), _) => BlockStart(s)
        case Stats(_,_,_,_,_,_,_,_,_, Some(_)) => BlockEnd(s)
        case _                                 => BlockBit(s)
      }
      totalContext(stats.map(toBlock)).toList
    }
    def total = totals.lastOption.getOrElse(Stats())
  }
  case object SpecsStatistics {
    def apply(current: Stats) = new SpecsStatistics(List(current))
  }
}
private [specs2]
object Statistics extends Statistics
/**
 * The Stats class store results for the number of:
 * * successes
 * * expectations
 * * failures
 * * errors
 * * pending
 * * skipped
 * 
 *  for each example
 *
 */
case class Stats(fragments:    Int = 0, 
                 successes:    Int = 0, 
                 expectations: Int = 0, 
                 failures:     Int = 0, 
                 errors:       Int = 0, 
                 pending:      Int = 0, 
                 skipped:      Int = 0,
                 timer:        SimpleTimer = new SimpleTimer,
                 start:        Option[ExecutedSpecStart] = None,
                 end:          Option[ExecutedSpecEnd] = None) {
  
  /** @return true if this Stats object is the SpecStart corresponding to the 'end' parameter */
  def isEnd(end: ExecutedSpecEnd) = {
    start.map(_.name == end.name).getOrElse(false)
  }
  /** @return true if there are errors or failures */
  def hasFailuresOrErrors = failures + errors > 0
  /** @return true if there are expectations */
  def hasExpectations = expectations > 0
  /** @return an equivalent result for display */
  def result =
    if (failures + errors == 0)
      if (successes > 0 || skipped + pending == 0) StandardResults.success
      else if (pending > skipped)                  StandardResults.pending
      else                                         StandardResults.skipped
    else if (errors > 0)           StandardResults.anError
    else                           StandardResults.failure

  /** @return true if there are no issues at all */
  def isSuccess = result.isSuccess
  /** @return true if there are failures or errors */
  def hasIssues = result.isFailure || result.isError

  def toXml = <stats fragments    = {fragments.toString}
                     successes    = {successes.toString}
                     expectations = {expectations.toString}
                     failures     = {failures.toString}
                     errors       = {errors.toString}
                     pending      = {pending.toString}
                     skipped      = {skipped.toString}
                     time         = {timer.elapsed.toString} />
}
case object Stats {
  implicit object StatsMonoid extends Monoid[Stats] {
    def append(s1: Stats, s2: =>Stats) = {
      s1.copy(
        fragments    = s1.fragments       + s2.fragments,
        successes    = s1.successes       + s2.successes,
        expectations = s1.expectations    + s2.expectations,
        failures     = s1.failures        + s2.failures,
        errors       = s1.errors          + s2.errors,
        pending      = s1.pending         + s2.pending,
        skipped      = s1.skipped         + s2.skipped,
        timer        = s1.timer           add s2.timer,
        start        = s1.start           orElse s2.start,
        end          = s2.end             orElse s1.end
      )
    }

    val zero = Stats()
  }

  def fromXml(stats: scala.xml.Node) = {
    if (stats.label != Stats().toXml.label)
      Stats()
    else {
      val map = stats.attributes.asAttrMap
      def asInt(key: String) = tryOrElse(Integer.parseInt(map(key)))(0)
      Stats(asInt("fragments"   ),
            asInt("successes"   ),
            asInt("expectations"),
            asInt("failures"    ),
            asInt("errors"      ),
            asInt("pending"     ),
            asInt("skipped"     ),
            map.get("time").map(SimpleTimer.fromString).getOrElse(new SimpleTimer))
    }

  }
}
