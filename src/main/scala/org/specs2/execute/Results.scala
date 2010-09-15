package org.specs2
package execute
import matcher._
import control.Exceptionx._

sealed abstract class Result(val message: String = "", val expectationsNb: Int = 1) {
  def and(r: Result): Result = this
  def status = this match {
	  case Success(_) => "+"
	  case Failure(_) => "x"
	  case Error(_)   => "x"
	  case Pending(_) => "o"
	  case Skipped(_) => "o"
	}
  def copyMessage(msg: String) = { 
	this match {
	  case Success(m) => Success(msg)
	  case Failure(m) => Failure(msg)
	  case Error(m)   => Error(msg)
	  case Skipped(m) => Skipped(msg)
	  case Pending(m) => Pending(msg)
	}
  }
}
case class Success(m: String = "")  extends Result(m) {
  override def and(r: Result): Result = r match {
	case Success(m) => Success(message+" and "+m)
	case Failure(e) => r
	case _ => super.and(r)
  }
}
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
  private def e = {
	if (!exception.getStackTrace.exists(_.toString matches "(org.specs2.*Spec|org.specs2.*Unit)"))
      exception.filter("org.specs2") 
    else 
      exception
  }
  def stackTrace = e.getStackTrace.toList
  def location = e.location
}
case class Pending(m: String = "")  extends Result(m) {
}
case class Skipped(m: String = "")  extends Result(m) {
}

