package org.specs2
package runner
import specification._

trait Statistics {
  type S
  val stats: Function[(S, ExecutedFragment), S]
}
trait TotalStatistics extends Statistics {
  type S = Stats
  case class Stats(examples: Int = 0, expectations: Int = 0, failures: Int = 0, errors: Int = 0, pending: Int = 0, skipped: Int = 0)

  val stats: Function[(S, ExecutedFragment), S] = {
	case (s, ExecutedResult(_, r)) => {
	  val u = s.copy(examples = s.examples + 1, expectations = s.expectations + r.expectationsNb)
	  r match {
	 	case Success(_) => u
	 	case Failure(_) => u.copy(failures = u.failures + 1)
	 	case Error(_) => u.copy(errors = u.errors + 1)
	 	case Pending(_) => u.copy(pending = u.pending + 1)
	 	case Skipped(_) => u.copy(skipped = u.skipped + 1)
	  }
	}
	case (s, other) => s
  }
}
