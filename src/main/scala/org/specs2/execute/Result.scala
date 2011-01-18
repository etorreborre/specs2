package org.specs2
package execute
import control.Throwablex
import control.Throwablex._
import text.AnsiColors._
import text.NotNullStrings._
import main.Arguments

/**
 * The result of an execution, either:
 *
 *  * a success: the execution is ok
 *  * a failure: an expectation is not met
 *  * an error: an exception occurred
 *  * a pending execution: the user has decided that execution must not be performed
 *  * a skipped execution: based on dynamic conditions (a database not available for instance)
 *    the execution is not performed 
 * 
 * A Result has:
 *  * a message describing the outcome
 *  * a message describing the expectation
 *  * possibly a number of expectations
 *    when it is the outcome of several checks (this is used for the reporting of ScalaCheck properties).
 * 
 */
sealed abstract class Result(val message: String = "", val expected: String = "", val expectationsNb: Int = 1) {
  /** @return the textual status of the result */
  def status(implicit args: Arguments = Arguments()) =
    if (args.plan) 
      color("*", blue, args.color)
    else {
      this match {
    	  case Success(_)          => color("+", green, args.color)
    	  case Failure(_, _, _, _) => color("x", yellow, args.color)
    	  case Error(_, _)         => color("!", red, args.color)
    	  case Pending(_)          => color("*", blue, args.color)
    	  case Skipped(_, _)       => color("o", cyan, args.color)
      }
    }
  
  /** @return the textual status of the result */
  def statusName(implicit args: Arguments = Arguments()) =
    if (args.plan)
      "info"
    else {
      this match {
    	  case Success(_)          => "success"
    	  case Failure(_, _, _, _) => "failure"
    	  case Error(_, _)         => "error"
    	  case Pending(_)          => "pending"
    	  case Skipped(_, _)       => "skipped"
      }
    }

  /** update the message of a result, keeping the subclass type */
  def updateMessage(msg: String) =
	  this match {
	    case Success(m)           => Success(msg)
	    case Failure(m, e, st, d) => Failure(msg, e, st, d)
	    case Error(m, st)         => Error(msg, st)
	    case Skipped(m, e)        => Skipped(msg, e)
	    case Pending(m)           => Pending(msg)
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
case class Success(m: String = "")  extends Result(m, m) {
  override def and(r: =>Result): Result = r match {
	  case Success(m) => if (message == m) this else Success(message+" and "+m)
	  case Failure(m, _, _, _) => r
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
case class Failure(m: String, e: String = "", stackTrace: List[StackTraceElement] = new Exception().getStackTrace.toList, details: Details = NoDetails())
  extends Result(m, e) with ResultStackTrace {
  /** @return an exception created from the message and the stackTraceElements */
  def exception = Throwablex.exception(m, stackTrace)
  override def or(r: =>Result): Result = r match {
    case Success(m) => if (message == m) r else Success(message+" and "+m)
    case Failure(m, e, st, d) => Failure(message+" and "+m, e, stackTrace ::: st, d)
    case _ => super.or(r)
  }
  override def toString = m
  override def equals(o: Any) = {
    o match {
      case Failure(m2, e2, _, _) => m == m2 && e == e2
      case _ => false
    }
  }
  override def hashCode = m.hashCode + e.hashCode
}

/**
 * Trait to model detailled information for failures so that smart differences can be computed
 */
sealed trait Details
case class FailureDetails(expected: String, actual: String) extends Details
case class NoDetails() extends Details
/** 
 * This class represents an exception occurring during an execution.
 */
case class Error(m: String, e: Exception)
  extends Result(m) with ResultStackTrace {
  /** @return an exception created from the message and the stackTraceElements */
  def exception = e
  def stackTrace = e.getFullStackTrace.toList
  override def equals(o: Any) = {
    o match {
      case Error(m2, e2) => m == m2 && e.getMessage == e2.getMessage
      case _ => false
    }
  }
  override def hashCode = m.hashCode
}
/** 
 * This object allows to create an Error from an exception
 */
case object Error {
  def apply(e: Exception) = new Error(e.getMessage.notNull, e)
  def apply(m: String) = new Error(m, new Exception(m))  
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
case class Skipped(m: String = "", e: String = "")  extends Result(m, e)
