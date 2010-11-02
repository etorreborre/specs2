package org.specs2
package reporter

import main.Arguments
import execute._
import specification._

private[specs2]
trait Statistics {
  type S
  def stats(implicit args: Arguments): Function[(S, ExecutedFragment), S]
}
trait TotalStatistics extends Statistics {
  type S = Stats
  case class Stats(Fragments: Int = 0, expectations: Int = 0, failures: Int = 0, errors: Int = 0, pending: Int = 0, skipped: Int = 0) {
    def hasFailuresOrErrors = failures + errors > 0
  }

  def stats(implicit args: Arguments): Function[(S, ExecutedFragment), S] = {
	  case (s, ExecutedResult(_, r)) => {
	    val u = s.copy(Fragments = s.Fragments + 1, expectations = s.expectations + r.expectationsNb)
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
