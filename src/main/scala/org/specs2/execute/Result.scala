package org.specs2
package execute

import control.Throwablex
import control.Throwablex._
import text.NotNullStrings._
import main.Arguments
import scalaz.Scalaz._
import scalaz.{Foldable, Monoid}
import Foldable._
import collection.Seqx._
import text.Message.concat
import text.Sentences._
import execute.ResultExecution._

/**
 * The result of an execution, either:
 *
 *  - a success: the execution is ok
 *  - a failure: an expectation is not met
 *  - an error: an exception occurred
 *  - a pending execution: the user has decided that execution must not be performed
 *  - a skipped execution: based on dynamic conditions (a database not available for instance)
 *    the execution is not performed 
 * 
 * A Result has:
 *  - a message describing the outcome
 *  - a message describing the expectation
 *  - possibly a number of expectations
 *    when it is the outcome of several checks (this is used for the reporting of ScalaCheck properties).
 * 
 */
sealed abstract class Result(val message: String = "", val expected: String = "", val expectationsNb: Int = 1) {
  /**
   * @return the colored textual status of the result
   */
  def coloredStatus(implicit args: Arguments = Arguments()): String = {
    if (args.plan)
      args.pendingColor("*")
    else {
      this match {
        case Success(_,_)          => args.successColor("+")
        case Failure(_, _, _, _)   => args.failureColor("x")
        case Error(_, _)           => args.errorColor  ("!")
        case Pending(_)            => args.pendingColor("*")
        case Skipped(_, _)         => args.skippedColor("o")
        case DecoratedResult(_, r) => r.coloredStatus(args)
      }
    }
  }

  private lazy val nocolor = Arguments("nocolor")

  /**
   * @return the uncolored textual status of the result
   */
  def status: String = coloredStatus(nocolor)

  /** @return the textual status of the result */
  def statusName(implicit args: Arguments = Arguments()): String =
    if (args.plan)
      "info"
    else {
      this match {
        case Success(_,_)          => "success"
        case Failure(_, _, _, _)   => "failure"
        case Error(_, _)           => "error"
        case Pending(_)            => "pending"
        case Skipped(_, _)         => "skipped"
        case DecoratedResult(_, r) => r.statusName(args)
      }
    }

  /** update the message of a result, keeping the subclass type */
  def updateMessage(msg: String): Result =
	  this match {
	    case Success(m, e)         => Success(msg,e)
	    case Failure(m, e, st, d)  => Failure(msg, e, st, d)
	    case Error(m, st)          => Error(msg, st)
	    case Skipped(m, e)         => Skipped(msg, e)
	    case Pending(m)            => Pending(msg)
      case DecoratedResult(t, r) => DecoratedResult(t, r.updateMessage(msg))
	  }

  /** change this result's message */
  def mapMessage(f: String => String): Result = updateMessage(f(message))

  /** update the expected of a result, keeping the subclass type */
  def updateExpected(exp: String): Result =
    this match {
      case Success(m, e)         => Success(m,exp, expectationsNb)
      case Failure(m, e, st, d)  => Failure(m, exp, st, d).setExpectationsNb(expectationsNb)
      case Skipped(m, e)         => Skipped(m, exp)
      case DecoratedResult(t, r) => DecoratedResult(t, r.updateExpected(exp))
      case other                 => this
    }

  /** change this result's expected */
  def mapExpected(f: String => String): Result = updateExpected(f(expected))

  /**
   * increment the number of expectations
   */
  def addExpectationsNb(n: Int): Result = setExpectationsNb(expectationsNb + n)
  /**
   * set the number of expectations
   */
  def setExpectationsNb(n: Int): Result

  /**
   * @return true if the result is a Success instance
   */
  def isSuccess: Boolean = false
  /**
   * @return true if the result is an Error instance
   */
  def isError: Boolean = false
  /**
   * @return true if the result is a Skipped instance
   */
  def isSkipped: Boolean = false
  /**
   * @return true if the result is a Pending instance
   */
  def isPending: Boolean = false
  /**
   * @return true if the result is a Skipped or Pending
   */
  def isSuspended: Boolean = isSkipped || isPending
  /**
   * @return true if the result is a Failure instance
   */
  def isFailure: Boolean = false
  /**
   * @return the result with no message
   */
  def mute: Result
}
object Result {

  /**
   * @return the accumulation of all results, without success messages
   */
  def issues(results: Seq[Result], separator: String = "; ") =
    results.foldMap(identity)(ResultFailuresMonoid(separator)).addExpectationsNb(-1)

  implicit val ResultMonoid: Monoid[Result] = new Monoid[Result] {
    val zero = Success()
    def append(m1: Result, m2: =>Result) = {
      (m1, m2) match {
        case (Success(msg1, e1),           Success(msg2, e2))          => Success(msg1+"; "+msg2, concat(e1, e2))
        case (Success(msg1, e1),           Skipped(msg2, e2))          => Success(msg1+"; "+msg2, e1)
        case (Skipped(msg1,  _),           Success(msg2, e2))          => Success(msg1+"; "+msg2, e2)
        case (Pending(msg1),               Success(msg2, e2))          => Success(msg1+"; "+msg2, e2)
        case (Success(msg1,e1),            Pending(msg2))              => Success(msg1+"; "+msg2, e1)

        case (Success(msg1, e1),           Failure(msg2, e2, st1, d2)) => m2.updateMessage(msg1+"; "+msg2)
        case (Failure(msg1, e1, st1, d1),  Failure(msg2, e2, st2, d2)) => Failure(msg1+"; "+msg2,
                                                                                  concat(e1, e2), st1, NoDetails())

        case (Success(msg1, e1),           Error(msg2, st1))           => m2.updateMessage(msg1+"; "+msg2)
        case (Error(msg1, st1),            Error(msg2, st2))           => Error(msg1+"; "+msg2, st1)
        case (Error(msg1, st1),            Failure(msg2, e2, st2, d2)) => Error(msg1+"; "+msg2, st1)

        case (Skipped(msg1, e1),           Skipped(msg2, e2))          => Skipped(msg1+"; "+msg2, e1+"; "+e2)
        case (Skipped(msg1, e1),           Pending(msg2))              => Pending(msg1+"; "+msg2)
        case (Pending(msg1),               Skipped(msg2, e2))          => Pending(msg1+"; "+msg2)
        case (Pending(msg1),               Pending(msg2))              => Pending(msg1+"; "+msg2)

        case (DecoratedResult(t, r),       other)                      => DecoratedResult(t, append(r, other))
        case (other,                       DecoratedResult(t, r))      => DecoratedResult(t, append(other, r))

        case (Failure(msg1, e1, st, d),    _)                          => m1
        case (Error(msg1, st),          _)                             => m1
        case (_,                           Failure(msg1, e1, st, d))   => m2
        case (_,                           Error(msg1, st))            => m2
      }
    }.setExpectationsNb(m1.expectationsNb + m2.expectationsNb)
  }
  /**
   * This monoids "absorbs" success messages if the result of the |+| is not a success
   */
  implicit val ResultFailureMonoid: Monoid[Result] = ResultFailuresMonoid("; ")
  implicit def ResultFailuresMonoid(separator: String): Monoid[Result] = new Monoid[Result] {
      val zero = Success()
    def append(m1: Result, m2: =>Result) = {
      (m1, m2) match {
        case (Success(msg1, e1),           Success(msg2, e2))          => Success("", concat(e1, e2))
        case (Success(msg1, e1),           other)                      => other
        case (other,                       Success(msg2, e2))          => other
        case (Failure(msg1, e1, st1, d1),  Failure(msg2, e2, st2, d2)) => Failure(concat(msg1, msg2, separator), e1+separator+e2, st1, NoDetails())
        case (Error(msg1, st1),            Error(msg2, st2))           => Error(concat(msg1, msg2, separator), st1)
        case (Error(msg1, st1),            Failure(msg2, e2, st2, d2)) => Error(concat(msg1, msg2, separator), st1)
        case (Skipped(msg1, e1),           Skipped(msg2, e2))          => Skipped(concat(msg1, msg2, separator), e1+separator+e2)
        case (Skipped(msg1, e1),           Pending(msg2))              => Pending(concat(msg1, msg2, separator))
        case (Pending(msg1),               Skipped(msg2, e2))          => Pending(concat(msg1, msg2, separator))
        case (Pending(msg1),               Pending(msg2))              => Pending(concat(msg1, msg2, separator))
        case (DecoratedResult(t, r),       other)                      => DecoratedResult(t, append(r, other))
        case (other,                       DecoratedResult(t, r))      => DecoratedResult(t, append(other, r))
        case (Failure(msg1, e1, st, d),    _)                          => m1
        case (Error(msg1, st),             _)                          => m1
        case (_,                           Failure(msg1, e1, st, d))   => m2
        case (_,                           Error(msg1, st))            => m2
      }
    }.setExpectationsNb(m1.expectationsNb + m2.expectationsNb)
  }

  /** the result of a side-effecting block */
  def unit(u: =>Unit) = ResultExecution.execute(u)(_ => Success())
}

trait Results {
  /**
   * implicit definition to accept any boolean value as a Result
   * This avoids writing b must beTrue
   */
  implicit def toResult(b: Boolean): Result =
    if (b) org.specs2.execute.Success("true") else org.specs2.execute.Failure("false")

  def negate(r: Result) = {
    if (r.isSuccess) Failure(negateSentence(r.message), r.expected).setExpectationsNb(r.expectationsNb)
    else if (r.isFailure) Failure(negateSentence(r.message), r.expected).setExpectationsNb(r.expectationsNb)
    else r
  }
}

object Results extends Results

/**
 * Typeclass trait for anything that can be transformed to a Result
 */
trait AsResult[T] {
  def asResult(t: =>T): Result
}

object AsResult {
  /** implicit typeclass instance to create examples from Results */
  implicit def resultAsResult[R <: Result]: AsResult[R] = new AsResult[R] {
    def asResult(t: =>R): Result = ResultExecution.execute(t)
  }
  /** implicit typeclass instance to create examples from Booleans */
  implicit def booleanAsResult: AsResult[Boolean] = new AsResult[Boolean] {
    def asResult(t: =>Boolean): Result = Results.toResult(t)
  }

  /** nicer syntax to use the AsResult syntax: AsResult(r) */
  def apply[R : AsResult](r: =>R): Result = implicitly[AsResult[R]].asResult(r)

  /** typeclass instance for types which are convertible to Result */
  implicit def asResult[R <% Result]: AsResult[R] = new AsResult[R] {
    def asResult(r: =>R): Result = ResultExecution.execute(r)
  }
}

/**
 * Type class to transform any value to a Result
 */
class AnyValueAsResult[T] extends AsResult[T] {
  def asResult(t: =>T) = {
    executeEither(t)(_.toString) match {
      case Left(e)  => new DecoratedResult((), e)
      case Right(v) => new DecoratedResult(v, Success())
    }
  }
}


/**
 * This class represents the success of an execution
 */
case class Success(m: String = "", exp: String = "")  extends Result(m, exp) {
  override def isSuccess = true

  def setExpectationsNb(n: Int): Result = Success(m, expected, n)

  def mute = Success()
}
/**
 * Companion object to the Success class providing 
 * a method to set the expectations number
 */
object Success {
  def apply(m: String, exp: String, expNb: Int) = new Success(m, exp) {
    override val expectationsNb = expNb
  }
  def apply(m: String, expNb: Int) = new Success(m) {
	  override val expectationsNb = expNb
  }
}
/** 
 * This class represents the failure of an execution.
 * It has a message and a stacktrace
 */
case class Failure(m: String = "", e: String = "", stackTrace: List[StackTraceElement] = new Exception().getStackTrace.toList, details: Details = NoDetails())
  extends Result(m, e) with ResultStackTrace { outer =>
  /** @return an exception created from the message and the stackTraceElements */
  def exception = Throwablex.exception(m, stackTrace)

  def setExpectationsNb(n: Int): Result = new Failure(m, e, stackTrace, details) {
    override val expectationsNb = n
  }
  def mute = copy(m = "",  e = "")

  override def toString = m
  override def equals(o: Any) = {
    o match {
      case Failure(m2, e2, _, _) => m == m2 && e == e2
      case _ => false
    }
  }
  override def hashCode = m.hashCode + e.hashCode
  override def isFailure: Boolean = true

  def skip: Skipped = Skipped(m, e)
}

/**
 * Trait to model detailed information for failures so that smart differences can be computed
 */
sealed trait Details
case class FailureDetails(expected: String, actual: String) extends Details
case class NoDetails() extends Details
/** 
 * This class represents an exception occurring during an execution.
 */
case class Error(m: String, e: Exception) extends Result(m) with ResultStackTrace { outer =>
  /** @return an exception created from the message and the stackTraceElements */
  def exception = e
  def stackTrace = e.getFullStackTrace.toList

  override def equals(o: Any) = {
    o match {
      case Error(m2, e2) => m == m2 && e.getMessage == e2.getMessage
      case _ => false
    }
  }

  def setExpectationsNb(n: Int): Result = new Error(m, e) {
    override val expectationsNb = n
  }
  def mute = copy(m = "")

  override def hashCode = m.hashCode
  override def isError: Boolean = true
}
/** 
 * This object allows to create an Error from an exception
 */
case object Error {
  def apply(e: Exception) = new Error(e.getMessage.notNull, e)
  def apply(t: Throwable) = new Error(t.getMessage.notNull, new ThrowableException(t))
  case class ThrowableException(t: Throwable) extends Exception(t.getClass.getSimpleName+": "+t.getMessage, t)
  def apply(m: String = "") = new Error(m, new Exception(m))
}
/** 
 * Pending result
 * @see Result for description
 */
case class Pending(m: String = "")  extends Result(m) { outer =>

  def mute = Pending()
  def setExpectationsNb(n: Int): Result = new Pending(m) {
    override val expectationsNb = n
  }

  override def isPending: Boolean = true
}
/** 
 * Skipped result
 * @see Result for description 
 */
case class Skipped(m: String = "", e: String = "")  extends Result(m, e) { outer =>

  def mute = Skipped()
  def setExpectationsNb(n: Int): Result = new Skipped(m) {
    override val expectationsNb = n
  }

  override def isSkipped: Boolean = true
}

/**
 * This result allows to embed additional data with a given result for further display
 *
 * Is is used to provide a way to display properly the data tables in the HtmlExporter
 */
case class DecoratedResult[+T](decorator: T, result: Result) extends Result(result.message, result.expected) { outer =>
  def mute = DecoratedResult(decorator, result.mute)
  def setExpectationsNb(n: Int): Result = new DecoratedResult(decorator, result) {
    override val expectationsNb = n
  }
    
  override def isSuccess: Boolean       = result.isSuccess
  override def isError: Boolean         = result.isError
  override def isSkipped: Boolean       = result.isSkipped
  override def isPending: Boolean       = result.isPending
  override def isFailure: Boolean       = result.isFailure

  /** use another decorator */
  def decorate[S](otherDecorator: S) = DecoratedResult(otherDecorator, result)
}
