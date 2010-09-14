package org.specs2
package execute
import matcher._
import control.Exceptionx._
sealed abstract class Result(val message: String = "", val expectationsNb: Int = 1)
case class Success(m: String = "")  extends Result(m)
case class Failure(e: Exception)  extends Result(e.getMessage) with HasStackTrace {
  val exception = e
}
case object Failure {
  def apply(m: String) = new Failure(new Exception(m))	
}
case class Error(e: Exception) extends Result(e.getMessage) with HasStackTrace {
  val exception = e
}
case object Error {
  def apply(m: String) = new Error(new Exception(m))	
}
trait HasStackTrace {
  val exception: Exception
  private def e = exception.removeTracesAsFarAsNameMatches("specification.BaseSpecification")
  def stackTrace = e.getStackTrace.toList
  def location = e.location
}
case class Pending(m: String = "")  extends Result(m)
case class Skipped(m: String = "")  extends Result(m)

