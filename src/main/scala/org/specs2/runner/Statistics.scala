package org.specs2
package runner
import specification._

trait Statistics {
  type S
  val stats: Function[(S, ExecutedFragment), S]
}
trait TotalStatistics extends Statistics {
  type S = Stats
  case class Stats(examplesNumber: Int = 0)

  val stats: Function[(S, ExecutedFragment), S] = {
	case (s, ExecutedResult(_, r)) => s.copy(examplesNumber = s.examplesNumber + 1)
	case (s, other) => s
  }

}
