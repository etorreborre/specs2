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
      SpecsStatistics(s1.fragments ++ s2.fragments)
    }
    val zero = SpecsStatistics() 
  }

  def foldAll(fs: Seq[ExecutedFragment]) = fs.foldMap(StatisticsReducer.unit)
  
  object StatisticsReducer extends Reducer[ExecutedFragment, SpecsStatistics] {
    override def unit(f: ExecutedFragment): SpecsStatistics = SpecsStatistics(f)
  }

  /**
   * The SpecsStatistics class stores the result of a specification execution, with the
   * a list of 'current' stats for each fragment execution and the total statistics 
   * for the whole specification
   */
  case class SpecsStatistics(fragments: List[ExecutedFragment] = Nil) {
    private implicit val statsMonoid = Stats.StatsMonoid
    
    /** @return the list of all current stats, with the total on each line */
    def totals: List[Stats] = {
      import NestedBlocks._

      def toBlock(f: ExecutedFragment) = f match {
        case ExecutedSpecStart(_,_,s)  => BlockStart(s)
        case ExecutedSpecEnd(_,_,s)    => BlockEnd(s)
        case other                     => BlockBit(f.stats)
      }
      totalContext(fragments.map(toBlock)).toList
    }
    def total = totals.lastOption.getOrElse(Stats())
  }
  case object SpecsStatistics {
    def apply(current: ExecutedFragment) = new SpecsStatistics(List(current))
  }

  /**
   * The SpecsStats class just stores a list of stats, each one corresponding to a Fragment
   */
  case class SpecStats(stats: List[Stats] = Nil) {
    def last = stats.lastOption.getOrElse(Stats())
  }
  implicit def SpecStatsMonoid  = new Monoid[SpecStats] {
    def append(s1: SpecStats, s2: =>SpecStats): SpecStats = {
      SpecStats(s1.stats ++ s2.stats)
    }
    val zero = SpecStats()
  }

  object StatsReducer extends Reducer[ExecutedFragment, SpecStats] {
    override def unit(f: ExecutedFragment): SpecStats = SpecStats(List(f.stats))
  }

}
private [specs2]
object Statistics extends Statistics
