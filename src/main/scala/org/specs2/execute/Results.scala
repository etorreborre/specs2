package org.specs2
package execute

sealed abstract class Result(val message: String = "", val expectationsNb: Int = 1)
case class Success(m: String = "")  extends Result(m)
case class Failure(e: Exception)  extends Result(e.getMessage) with HasStackTrace {
  def stackTrace = e.getStackTrace.toList
}
case object Failure {
  def apply(m: String) = new Failure(new Exception(m))	
}
case class Error(e: Throwable) extends Result(e.getMessage) with HasStackTrace {
  def stackTrace = e.getStackTrace.toList
}
case object Error {
  def apply(m: String) = new Error(error(m))	
}
trait HasStackTrace {
  def stackTrace: List[StackTraceElement]
}
case class Pending(m: String = "")  extends Result(m)
case class Skipped(m: String = "")  extends Result(m)

trait MatchResult {
  def not: Result with MatchResult
  def or(m: =>Result with MatchResult): Result with MatchResult
}
class MatchSuccess(val okMessage: String, val koMessage: String) extends Success(okMessage) with MatchResult {
  def not = new MatchFailure(koMessage, okMessage)
  def or(m: =>Result with MatchResult) = this
}
class MatchFailure(val koMessage: String, val okMessage: String) extends Failure(new Exception(koMessage)) with MatchResult { outer =>
  def not = new MatchSuccess(koMessage, okMessage)
  def or(m: =>Result with MatchResult): Result with MatchResult  = m match {
	case s: MatchSuccess => new MatchSuccess(s.okMessage+" but "+ outer.koMessage, s.koMessage)
	case f: MatchFailure => new MatchFailure(f.koMessage+" and "+ outer.koMessage, f.okMessage + " and " + outer.okMessage)
  } 
}