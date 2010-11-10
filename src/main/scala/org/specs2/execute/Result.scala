package org.specs2
package execute
import control.Exceptionx
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
 * A Result has a message describing it more precisely and possibly a number of expectations
 * when it is the outcome of several checks (this is used for the reporting of ScalaCheck 
 * properties.
 * 
 */
sealed abstract class Result(val message: String = "", val expectationsNb: Int = 1) {
  /** @return the textual status of the result */
  def status = this match {
	  case Success(_)    => "+"
	  case Failure(_, _) => "x"
	  case Error(_, _)   => "!"
	  case Pending(_)    => "*"
	  case Skipped(_)    => "o"
  }
  /** update the message of a result, keeping the subclass type */
  def updateMessage(msg: String) = { 
	  this match {
	    case Success(m) => Success(msg)
	    case Failure(m, st) => Failure(msg, st)
	    case Error(m, st)   => Error(msg, st)
	    case Skipped(m) => Skipped(msg)
	    case Pending(m) => Pending(msg)
	  }
  }
  /**
   * @return the logical and combination of 2 results
   */
  def and(r: =>Result): Result = this
  /**
   * @return the logical or combination of 2 results
   */
  def or(r2: =>Result) = this 
   
  /**
   * @return true if the result is a Success instance
   */
  def isSuccess: Boolean = false
}
/** 
 * This class represents the success of an execution
 */
case class Success(m: String = "")  extends Result(m) {
  override def and(r: =>Result): Result = r match {
	  case Success(m) => if (message == m) this else Success(message+" and "+m)
	  case Failure(m, st) => r
	  case _ => super.and(r)
  }
  override def isSuccess = true
}
/**
 * Companion object to the Success class providing 
 * a method to set the expectations number
 */
object Success {
  def apply(m: String, expNb: Int) = new Success(m) {
	  override val expectationsNb = expNb
  }
}
/** 
 * This class represents the failure of an execution.
 * It has a message and a stacktrace
 */
case class Failure(m: String, stackTrace: List[StackTraceElement] = new Exception().getStackTrace.toList) 
  extends Result(m) with ResultStackTrace {
  /** @return an exception created from the message and the stackTraceElements */
  def exception = Exceptionx.exception(m, stackTrace)
  override def or(r: =>Result): Result = r match {
    case Success(m) => if (message == m) r else Success(message+" and "+m)
    case Failure(m, st) => Failure(message+" and "+m, stackTrace ::: st)
    case _ => super.or(r)
  }
  override def toString = m
  override def equals(o: Any) = {
    o match {
      case Failure(m2, _) => m == m2
      case _ => false
    }
  }
}
/** 
 * This class represents an exception occurring during an execution.
 */
case class Error(m: String, stackTrace: List[StackTraceElement] = new Exception().getStackTrace.toList) 
  extends Result(m) with ResultStackTrace {
  /** @return an exception created from the message and the stackTraceElements */
  def exception = Exceptionx.exception(m, stackTrace)
}
/** 
 * This object allows to create an Error from an exception
 */
case object Error {
  def apply(e: Exception) = new Error(e.getMessage, e.getStackTrace.toList)	
}
/** 
 * Pending result
 * @see Result for description
 */
case class Pending(m: String = "")  extends Result(m)
/** 
 * Skipped result
 * @see Result for description 
 */
case class Skipped(m: String = "")  extends Result(m)

