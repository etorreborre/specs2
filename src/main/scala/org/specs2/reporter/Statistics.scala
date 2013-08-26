package org.specs2
package reporter

import scalaz.{ Scalaz, Monoid, Reducer }
import Scalaz._
import collection.Seqx._
import specification._
import scala.collection.mutable.ArrayBuffer

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
  
  val StatisticsReducer: Reducer[ExecutedFragment, SpecsStatistics]  = Reducer.unitReducer {
    f: ExecutedFragment => SpecsStatistics(f)
  }

  /**
   * The SpecsStatistics class stores the result of a specification execution, with the
   * a list of 'current' stats for each fragment execution and the total statistics 
   * for the whole specification
   */
  case class SpecsStatistics(fragments: Seq[ExecutedFragment] = ArrayBuffer()) {
    private val statsMonoid = Stats.StatsMonoid
    
    /** @return the list of all current stats, with the total on each line */
    lazy val totals: Seq[Stats] = {
      import NestedBlocks._

      def toBlock(f: ExecutedFragment) = f match {
        case ExecutedSpecStart(_,_,s)  => BlockStart(s)
        case ExecutedSpecEnd(_,_,s)    => BlockEnd(s)
        case other                     => BlockBit(f.stats)
      }
      totalContext(fragments.map(toBlock))(statsMonoid)
    }
    lazy val total = totals.lastOption.getOrElse(Stats())
  }
  case object SpecsStatistics {
    def apply(current: ExecutedFragment) = new SpecsStatistics(ArrayBuffer(current))
  }

  /**
   * The SpecsStats class just stores a list of stats, each one corresponding to a Fragment
   */
  case class SpecStats(stats: Seq[Stats] = ArrayBuffer()) {
    def last = stats.lastOption.getOrElse(Stats())
  }
  implicit def SpecStatsMonoid  = new Monoid[SpecStats] {
    def append(s1: SpecStats, s2: =>SpecStats): SpecStats = {
      SpecStats(s1.stats ++ s2.stats)
    }
    val zero = SpecStats()
  }

  val StatsReducer: Reducer[ExecutedFragment, SpecStats] = Reducer.unitReducer {
    f: ExecutedFragment => SpecStats(ArrayBuffer(f.stats))
  }

}
private [specs2]
object Statistics extends Statistics
