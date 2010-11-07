package org.specs2
package reporter

import main.Arguments
import execute._
import specification._

/**
 * This trait describe any kind of statistics which can be accumulated on executed fragments
 * This can be the total number of successes, but also the execution time
 */
private[specs2]
trait Statistics {
  type S
  def stats(implicit args: Arguments): Function[(S, ExecutedFragment), S]
}

/**
 * This Statistics trait computes the total number of:
 * * successes
 * * expectations
 * * failures
 * * errors
 * * pending
 * * skipped
 * 
 * examples
 *
 */
trait TotalStatistics extends Statistics {
  type S = Stats

  case class Stats(fragments: Int = 0, expectations: Int = 0, failures: Int = 0, errors: Int = 0, pending: Int = 0, skipped: Int = 0) {
    def hasFailuresOrErrors = failures + errors > 0
  }

  /**
   * @return a function computing the statistics depending on the existing number and 
   * the current executed fragment
   */
  def stats(implicit args: Arguments): Function[(S, ExecutedFragment), S] = {
	  case (s, ExecutedResult(_, r)) => {
	    val u = s.copy(fragments = s.fragments + 1, expectations = s.expectations + r.expectationsNb)
	    r match {
	   	  case Success(_) => u
	   	  case Failure(_, _) => u.copy(failures = u.failures + 1)
	   	  case Error(_,_) => u.copy(errors = u.errors + 1)
	   	  case Pending(_) => u.copy(pending = u.pending + 1)
	   	  case Skipped(_) => u.copy(skipped = u.skipped + 1)
	    }
	  }
	  case (s, other) => s
  }
}
