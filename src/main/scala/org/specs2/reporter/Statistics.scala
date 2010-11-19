package org.specs2
package reporter

import scalaz.{ Scalaz, Monoid, Reducer }
import Scalaz._
import main.Arguments
import execute._
import specification._

/**
 * This Statistics trait computes the total number of:
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
trait Statistics extends ExecutedFragmentFold {
  type T = (Stats, Stats)
  def initial = (Stats(), Stats())


  /**
   * @return a function computing the statistics depending on the existing number and 
   * the current executed fragment
   */
  def fold(implicit args: Arguments): Function2[T, ExecutedFragment, T] = {
	  case ((total, current), ExecutedResult(_, r)) => {
	    val newCurrent = current.copy(fragments = current.fragments + 1, 
	                                  expectations = current.expectations + r.expectationsNb)
	    val updatedCurrent = r match {
        case Success(_)    => newCurrent.copy(successes = current.successes + 1)
	   	  case Failure(_, _) => newCurrent.copy(failures = current.failures + 1)
	   	  case Error(_,_)    => newCurrent.copy(errors =   current.errors + 1)
	   	  case Pending(_)    => newCurrent.copy(pending =  current.pending + 1)
	   	  case Skipped(_)    => newCurrent.copy(skipped =  current.skipped + 1)
	   	  case _             => newCurrent                 
	    }
	    (total, updatedCurrent)
	  }
	  case ((total, current), start @ ExecutedSpecStart(name, timer, args)) => 
	                                (total.copy(start = total.start.orElse(Some(ExecutedSpecStart(name, timer.stop, args)))), Stats())
    case ((total, current), ExecutedSpecEnd(_)) => (total add current, current)
    case ((total, current), _) => (total, current)
  }
}
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
    def add(s: Stats) = copy(
        fragments = this.fragments       + s.fragments,
        successes = this.successes       + s.successes, 
        expectations = this.expectations + s.expectations, 
        failures = this.failures         + s.failures   , 
        errors = this.errors             + s.errors     , 
        pending = this.pending           + s.pending    , 
        skipped = this.skipped           + s.skipped    )
  }

case class SpecsStatistics(currents: List[Stats] = Nil, total: Stats = Stats()) {
  def current = currents.lastOption.getOrElse(Stats())
  def toList = currents.map((_, total))
}
case object SpecsStatistics {
  def apply(current: Stats) = new SpecsStatistics(List(current), current)
}
private[specs2]
object Statistics extends Statistics {
  implicit object StatsMonoid extends Monoid[SpecsStatistics] {
    def append(s1: SpecsStatistics, s2: =>SpecsStatistics): SpecsStatistics = {
      SpecsStatistics(s1.currents ++ s2.currents, s1.total.add(s2.total))
    }
    val zero = SpecsStatistics() 
  }
  implicit object ExecutedFragmentsStatisticsReducer extends Reducer[ExecutedFragment, SpecsStatistics] {
    implicit override def unit(f: ExecutedFragment): SpecsStatistics = f match { 
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
}
