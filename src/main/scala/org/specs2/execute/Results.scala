package org.specs2
package execute
import control.Exceptionx._

/**
 * The result of an execution, either:
 *  * a success: the execution is ok
 *  * a failure: an expectation is not met
 *  * an error: an exception occurred
 *  * a pending execution: the user has decided that execution must not be performed
 *  * a skipped execution: based on dynamic conditions (a database not available for instance)
 *    the execution is not performed 
 * 
 */
sealed abstract class Result(val message: String = "", val expectationsNb: Int = 1) {
  /** @return the textual status of the result */
  def status = this match {
	case Success(_) => "+"
	case Failure(_, _) => "x"
	case Error(_)   => "!"
	case Pending(_) => "*"
	case Skipped(_) => "o"
  }
  /** update the message of a result, keeping the subclass type */
  def updateMessage(msg: String) = { 
	this match {
	  case Success(m) => Success(msg)
	  case Failure(m, st) => Failure(msg, st)
	  case Error(e)   => Error(msg)
	  case Skipped(m) => Skipped(msg)
	  case Pending(m) => Pending(msg)
	}
  }
  def and(r: Result): Result = this
}
/** 
 * This class represents the success of an execution
 */
case class Success(m: String = "")  extends Result(m) {
  override def and(r: Result): Result = r match {
	case Success(m) => Success(message+" and "+m)
	case Failure(m, st) => r
	case _ => super.and(r)
  }
}
/** 
 * This class represents the failure of an execution.
 * It has a message and a stacktrace
 */
case class Failure(m: String, stackTrace: List[StackTraceElement] = new Exception().getStackTrace.toList) 
  extends Result(m) with ResultStackTrace
/** 
 * This class represents an exception occurring during an execution.
 */
case class Error(exception: Exception) extends Result(exception.getMessage) with ResultStackTrace {
  val stackTrace = exception.getStackTrace.toList
}
/** 
 * This object allow to create an Error from a message
 */
case object Error {
  def apply(m: String) = new Error(new Exception(m))	
}
case class Pending(m: String = "")  extends Result(m) {
}
case class Skipped(m: String = "")  extends Result(m) {
}

