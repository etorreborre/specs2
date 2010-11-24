package org.specs2
package reporter

import scalaz.{ Scalaz, Monoid, Reducer }
import Scalaz._
import collection.Iterablex._
import main.Arguments
import execute._
import specification._

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
                 start:        Option[ExecutedSpecStart] = None,
                 end:          Option[ExecutedSpecEnd] = None) {
  
  def isEnd(end: ExecutedSpecEnd) = {
    start.map(_.name == end.name).getOrElse(false)
  }
  /** @return true if there are errors or failures */
  def hasFailuresOrErrors = failures + errors > 0
}
case object Stats {
  implicit object StatsMonoid extends Monoid[Stats] {
    def append(s1: Stats, s2: =>Stats) = s1.copy(
        fragments =    s1.fragments       + s2.fragments,
        successes =    s1.successes       + s2.successes, 
        expectations = s1.expectations    + s2.expectations, 
        failures =     s1.failures        + s2.failures   , 
        errors =       s1.errors          + s2.errors     , 
        pending =      s1.pending         + s2.pending    , 
        skipped =      s1.skipped         + s2.skipped    )

    val zero = Stats()
  }
}
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
      case ExecutedResult(_, r) => {
        val current = r match {
          case s @ Success(_) => Stats(fragments = 1, expectations = s.expectationsNb, successes = 1)
          case Failure(_, _)  => Stats(fragments = 1, expectations = 1, failures = 1)
          case Error(_,_)     => Stats(fragments = 1, expectations = 1, errors = 1)
          case Pending(_)     => Stats(fragments = 1, expectations = 1, pending = 1)
          case Skipped(_)     => Stats(fragments = 1, expectations = 1, skipped = 1)
          case _              => Stats(fragments = 1) 
        }
        SpecsStatistics(current)
      }
      case start @ ExecutedSpecStart(name, timer, args) => 
        SpecsStatistics(Stats(start = Some(ExecutedSpecStart(name, timer.stop, args))))
      case e @ ExecutedSpecEnd(_) => SpecsStatistics(Stats(end = Some(e)))
      case _ => SpecsStatistics(Stats())
    }
  }
  /**
   * The SpecsStatistics class stores the result of a specification execution, with the
   * a list of 'current' stats for each fragment execution and the total statistics 
   * for the whole specification
   */
  case class SpecsStatistics(stats: List[Stats] = Nil) {
    import Stats._
    
    def toList = currents.map((_, total))
    def currents = stats.foldLeft(Nil: List[Stats]) { (res, cur) => 
      cur.start match {
        case Some(s) => res :+ Stats()
        case None    => res :+ (res.lastOption.getOrElse(Stats()) |+| cur)
      }
    }
    def total = stats.foldMap(identity)
    def current = currents.lastOption.getOrElse(Stats())
  }
  case object SpecsStatistics {
    def apply(current: Stats) = new SpecsStatistics(List(current))
  }
}
private [specs2]
object Statistics extends Statistics
