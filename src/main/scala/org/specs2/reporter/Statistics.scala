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
        def resultStats(result: Result): Stats = Stats(result)
        
        val current = resultStats(r)
        SpecsStatistics(current.copy(timer = t))
      }
      case start @ ExecutedSpecStart(name, args, _) => SpecsStatistics(Stats(start = Some(start)))
      case end @ ExecutedSpecEnd(_, _)              => SpecsStatistics(Stats(end = Some(end)))
      case ExecutedSee(_, _, _, stats, _)           => SpecsStatistics(stats)
      case ExecutedNoText(t, _)                     => SpecsStatistics(Stats(timer = t))
      case _                                        => SpecsStatistics(Stats())
    }
  }

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
