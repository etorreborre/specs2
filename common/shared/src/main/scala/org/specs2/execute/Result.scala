package org.specs2
package execute

import control.Throwablex
import control.Throwablex.*
import main.Arguments
import org.specs2.fp.*
import org.specs2.fp.syntax.*
import text.Message.concat
import text.Sentences.*
import text.NotNullStrings.*

/** The result of an execution, either:
  *
  *   - a success: the execution is ok
  *   - a failure: an expectation is not met
  *   - an error: an exception occurred
  *   - a pending execution: the user has decided that execution must not be performed
  *   - a skipped execution: based on dynamic conditions (a database not available for instance) the execution is not
  *     performed
  *
  * A Result has:
  *   - a message describing the outcome
  *   - a message describing the expectation
  *   - possibly a number of expectations when it is the outcome of several checks (this is used for the reporting of
  *     ScalaCheck properties).
  */
sealed abstract class Result(val message: String = "", val expected: String = "") derives CanEqual:
  type SelfType <: Result

  /** number of expectations for a Result The default number is 1 but this could be higher if this result represents the
    * execution of a ScalaCheck property
    */
  def expectationsNb: Int = 1

  /** @return
    *   the colored textual status of the result
    */
  def coloredStatus(using args: Arguments = Arguments()): String =
    if args.plan then args.pendingColor("*")
    else
      this match
        case Success(_, _)         => args.successColor("+")
        case Failure(_, _, _, _)   => args.failureColor("x")
        case Error(_, _)           => args.errorColor("!")
        case Pending(_)            => args.pendingColor("*")
        case Skipped(_, _)         => args.skippedColor("o")
        case DecoratedResult(_, r) => r.coloredStatus

  private lazy val nocolor = Arguments("nocolor")

  /** @return
    *   the uncolored textual status of the result
    */
  def status: String = coloredStatus(using nocolor)

  /** @return the textual status of the result */
  def statusName(using args: Arguments = Arguments()): String =
    if args.plan then "info"
    else
      this match
        case Success(_, _)         => "success"
        case Failure(_, _, _, _)   => "failure"
        case Error(_, _)           => "error"
        case Pending(_)            => "pending"
        case Skipped(_, _)         => "skipped"
        case DecoratedResult(_, r) => r.statusName

  /** set the message of a result, keeping the subclass type */
  def setMessage(msg: String): Result =
    this match
      case Success(m, e)         => Success(msg, e)
      case Failure(m, e, st, d)  => Failure(msg, e, st, d)
      case Error(m, st)          => Error(msg, st)
      case Skipped(m, e)         => Skipped(msg, e)
      case Pending(m)            => Pending(msg)
      case DecoratedResult(t, r) => DecoratedResult(t, r.setMessage(msg))

  /** change this result's message */
  def updateMessage(f: String => String): Result =
    setMessage(f(message))

  /** prepend another message and a conjunction depending on the status of this result */
  def prependMessage(pre: String): Result =
    updateMessage(m => s"$pre ${if isSuccess then "and" else "but"} " + m)

  /** update the expected of a result, keeping the subclass type */
  def updateExpected(exp: String): Result =
    this match
      case Success(m, e)         => Success(m, exp, expectationsNb)
      case Failure(m, e, st, d)  => Failure(m, exp, st, d).setExpectationsNb(expectationsNb)
      case Skipped(m, e)         => Skipped(m, exp)
      case DecoratedResult(t, r) => DecoratedResult(t, r.updateExpected(exp))
      case other                 => this

  /** change this result's expected */
  def mapExpected(f: String => String): Result = updateExpected(f(expected))

  /** increment the number of expectations
    */
  def addExpectationsNb(n: Int): SelfType =
    setExpectationsNb(expectationsNb + n)

  /** set the number of expectations
    */
  def setExpectationsNb(n: Int): SelfType

  /** @return
    *   true if the result is a Success instance
    */
  def isSuccess: Boolean = false

  /** @return
    *   true if the result is a failure or an Error
    */
  def isIssue: Boolean = isFailure || isError

  /** @return
    *   true if the result is an Error instance
    */
  def isError: Boolean = false

  /** @return
    *   true if the result is a Skipped instance
    */
  def isSkipped: Boolean = false

  /** @return
    *   true if the result is a Pending instance
    */
  def isPending: Boolean = false

  /** @return
    *   true if the result is a Skipped or Pending
    */
  def isSuspended: Boolean = isSkipped || isPending

  /** @return
    *   true if the result is a Failure instance
    */
  def isFailure: Boolean = false

  /** @return
    *   true if the result is a Failure that was thrown like a JUnit assertion error or a NotImplemented exception
    */
  def isThrownFailure: Boolean = false

  /** @return
    *   the result with no message
    */
  def mute: Result

  /** @return
    *   the negation of this result where a success becomes a failure and vice-versa
    */
  def negate: Result =
    this match
      case Success(m, e) =>
        Failure(s"Expectation failed: '$m'", e)
      case Failure(m, e, _, _) =>
        Success(m)
      case other =>
        other

object Result:

  /** @return a Success or a Failure */
  def result(
      test: Boolean,
      successMessage: =>String,
      failureMessage: =>String,
      expected: =>String,
      details: Details
  ): Result =
    if test then Success(successMessage.notNull)
    else Failure(m = failureMessage.notNull, e = expected, details = details)

  /** @return a Success or a Failure */
  def result(test: Boolean, message: =>String, expected: =>String, details: Details): Result =
    if test then Success(message.notNull)
    else Failure(m = message.notNull, e = expected, details = details)

  /** @return a Success or a Failure */
  def result(test: Boolean, message: =>String, details: Details): Result =
    result(test, message, "", details)

  /** @return a Success or a Failure */
  def result(test: Boolean, message: =>String, expected: String, actual: String): Result =
    result(test, message, FailureDetails(actual, expected))

  /** @return a Success or a Failure */
  def result(test: Boolean, message: =>String): Result =
    result(test, message, NoDetails)

  /** extract failure details from a Result if it is a Failure */
  def details(r: Result): Details = r match
    case f: Failure => f.details
    case _          => NoDetails

  /** @return
    *   the accumulation of all results, without success messages
    */
  def issues(results: scala.collection.Seq[Result], separator: String = "; ") =
    given Monoid[Result] = ResultFailuresMonoid(separator)
    results.toList.suml.addExpectationsNb(-1)

  /** This monoids keeps success messages if the result of the |+| is not a success
    */
  val ResultMonoid: Monoid[Result] = new Monoid[Result] {
    val zero = Success()
    def append(mr1: Result, mr2: =>Result) =
      val (m1, m2) = (mr1, mr2)
      ((m1, m2) match {
        case (Success(msg1, e1), Success(msg2, e2)) => Success(msg1 + "; " + msg2, concat(e1, e2))
        case (Success(msg1, e1), Skipped(msg2, e2)) => Success(msg1 + "; " + msg2, e1)
        case (Skipped(msg1, _), Success(msg2, e2))  => Success(msg1 + "; " + msg2, e2)
        case (Pending(msg1), Success(msg2, e2))     => Success(msg1 + "; " + msg2, e2)
        case (Success(msg1, e1), Pending(msg2))     => Success(msg1 + "; " + msg2, e1)

        case (Success(msg1, e1), Failure(msg2, e2, st1, d2))          => m2.setMessage(msg1 + "; " + msg2)
        case (Failure(msg1, e1, st1, d1), Failure(msg2, e2, st2, d2)) =>
          Failure(msg1 + "; " + msg2, concat(e1, e2), st1, NoDetails)

        case (Success(msg1, e1), Error(msg2, st1))          => m2.setMessage(msg1 + "; " + msg2)
        case (Error(msg1, st1), Error(msg2, st2))           => Error(msg1 + "; " + msg2, st1)
        case (Error(msg1, st1), Failure(msg2, e2, st2, d2)) => Error(msg1 + "; " + msg2, st1)

        case (Skipped(msg1, e1), Skipped(msg2, e2)) => Skipped(msg1 + "; " + msg2, e1 + "; " + e2)
        case (Skipped(msg1, e1), Pending(msg2))     => Pending(msg1 + "; " + msg2)
        case (Pending(msg1), Skipped(msg2, e2))     => Pending(msg1 + "; " + msg2)
        case (Pending(msg1), Pending(msg2))         => Pending(msg1 + "; " + msg2)

        case (DecoratedResult(t, r), other) => DecoratedResult(t, append(r, other))
        case (other, DecoratedResult(t, r)) => DecoratedResult(t, append(other, r))

        case (Failure(msg1, e1, st, d), _) => m1
        case (Error(msg1, st), _)          => m1
        case (_, Failure(msg1, e1, st, d)) => m2
        case (_, Error(msg1, st))          => m2
      }).setExpectationsNb(m1.expectationsNb + m2.expectationsNb)
  }

  /** This monoids "absorbs" success messages if the result of the |+| is not a success
    */
  given ResultFailureMonoid: Monoid[Result] = ResultFailuresMonoid("; ")

  def ResultFailuresMonoid(separator: String): Monoid[Result] = new Monoid[Result] {
    val zero = Success()

    def append(mr1: Result, mr2: =>Result) =
      val (m1, m2) = (mr1, mr2)
      ((m1, m2) match {
        case (Success(msg1, e1), Success(msg2, e2))                          => Success("", concat(e1, e2))
        case (Success(msg1, e1), other)                                      => other
        case (other, Success(msg2, e2))                                      => other
        case (Failure(msg1, e1, st1, d1), Failure(msg2, e2, st2, NoDetails)) =>
          Failure(concat(msg1, msg2, separator), e1 + separator + e2, st1, d1)
        case (Failure(msg1, e1, st1, NoDetails), Failure(msg2, e2, st2, d2)) =>
          Failure(concat(msg1, msg2, separator), e1 + separator + e2, st2, d2)
        case (Failure(msg1, e1, st1, d1), Failure(msg2, e2, st2, d2)) =>
          Failure(concat(msg1, msg2, separator), e1 + separator + e2, st1, d1)
        case (Error(msg1, st1), Error(msg2, st2))           => Error(concat(msg1, msg2, separator), st1)
        case (Error(msg1, st1), Failure(msg2, e2, st2, d2)) => Error(concat(msg1, msg2, separator), st1)
        case (Skipped(msg1, e1), Skipped(msg2, e2)) => Skipped(concat(msg1, msg2, separator), e1 + separator + e2)
        case (Skipped(msg1, e1), Pending(msg2))     => Pending(concat(msg1, msg2, separator))
        case (Pending(msg1), Skipped(msg2, e2))     => Pending(concat(msg1, msg2, separator))
        case (Pending(msg1), Pending(msg2))         => Pending(concat(msg1, msg2, separator))
        case (DecoratedResult(t, r), other)         => DecoratedResult(t, append(r, other))
        case (other, DecoratedResult(t, r))         => DecoratedResult(t, append(other, r))
        case (Failure(msg1, e1, st, d), _)          => m1
        case (Error(msg1, st), _)                   => m1
        case (_, Failure(msg1, e1, st, d))          => m2
        case (_, Error(msg1, st))                   => m2
      }).setExpectationsNb(m1.expectationsNb + m2.expectationsNb)
  }

  /** This monoid only evaluates the second argument
    * @return
    */
  val ResultShortCircuitMonoid: Monoid[Result] = new Monoid[Result] {
    val zero = Success()

    def append(mr1: Result, mr2: =>Result): Result =
      if mr1.isIssue then mr1
      else ResultFailureMonoid.append(mr1, mr2)
  }

  /** the result of a side-effecting block */
  def unit(u: =>Unit) = ResultExecution.effectively { u; Success() }

  /** implicit typeclass instance to create examples from Results */
  given resultAsResult[R <: Result]: AsResult[R] with
    def asResult(t: =>R): Result =
      ResultExecution.execute(t)

  def resultOrSuccess(t: Any): Result = t.asInstanceOf[Matchable] match
    case r: Result => r
    case _         => Success()

  def disjunctionErrorToResult(error: Throwable Either String): Result =
    error.fold(t => Error(t), m => Error(m))

  /** this allows the creation of expectations with a for loop it returns the first result which is an issue or the last
    * success
    */
  def foreach[T, R: AsResult](seq: Seq[T])(f: T => R): Result =
    given Monoid[Result] = ResultShortCircuitMonoid
    seq.toList.foldMap((t: T) => AsResult(f(t)))

  /** this returns a result which is a summary of all the results according to the ResultFailureMonoid
    */
  def forall[T, R: AsResult](seq: Seq[T])(f: T => R): Result =
    given Monoid[Result] = ResultFailureMonoid
    seq.toList.foldMap((t: T) => AsResult(f(t)))

  /** this returns a result which is a summary of all the results according to the ResultFailureMonoid
    */
  def forallWhen[T, R: AsResult](seq: Seq[T])(f: PartialFunction[T, R]): Result =
    given Monoid[Result] = ResultFailureMonoid
    seq.toList.foldMap((t: T) => AsResult(f(t)))

trait Results:
  /** implicit definition to accept any boolean value as a Result This avoids writing b must beTrue
    */
  given Conversion[Boolean, Result] with
    def apply(b: Boolean): Result =
      toResult(b)

  def toResult(b: Boolean): Result =
    if b then org.specs2.execute.Success("true")
    else org.specs2.execute.Failure("false", "true", Nil, NoDetails)

  def negate(r: Result) =
    if r.isSuccess then Failure(negateSentence(r.message), r.expected).setExpectationsNb(r.expectationsNb)
    else if r.isFailure then Success(negateSentence(r.message), r.expected).setExpectationsNb(r.expectationsNb)
    else r

  def negateWhen(condition: Boolean)(r: Result) =
    if condition then negate(r) else r

object Results extends Results

/** This class represents the success of an execution
  */
case class Success(m: String = "", exp: String = "") extends Result(m, exp):
  type SelfType = Success

  override def isSuccess = true

  def setExpectationsNb(n: Int) =
    new Success(m, expected) {
      override def expectationsNb: Int = n
    }

  def mute = Success()

  override def toString: String =
    "Success"

  override def equals(o: Any): Boolean =
    o.asInstanceOf[Matchable] match
      case Success(m2, e2) => m == m2 && exp == e2
      case _               => false

  override def hashCode: Int =
    m.hashCode + exp.hashCode

/** Companion object to the Success class providing a method to set the expectations number
  */
object Success:
  def apply(m: String, exp: String, expNb: Int) =
    new Success(m, exp).setExpectationsNb(expNb)

  def apply(m: String, expNb: Int) =
    new Success(m).setExpectationsNb(expNb)

/** This class represents the failure of an execution. It has a message and a stacktrace
  */
case class Failure(
    m: String = "",
    e: String = "",
    trace: List[StackTraceElement] = new Exception().getStackTrace.toList,
    details: Details = NoDetails
) extends Result(m, e)
    with ResultStackTrace:
  outer =>

  type SelfType = Failure

  def stackTrace: List[StackTraceElement] =
    trace

  /** @return an exception created from the message and the stackTraceElements */
  def exception = Throwablex.exception(m, trace)

  def setExpectationsNb(n: Int): Failure =
    new Failure(m, e, trace, details) {
      override def expectationsNb: Int = n
    }

  def mute: Failure =
    copy(m = "", e = "")

  override def toString = m
  override def equals(o: Any) =
    o.asInstanceOf[Matchable] match
      case Failure(m2, e2, _, _) => m == m2 && e == e2
      case _                     => false
  override def hashCode = m.hashCode + e.hashCode
  override def isFailure: Boolean = true

  override def isThrownFailure: Boolean =
    Seq(FromNotImplementedError, FromJUnitAssertionError, FromExpectationError, FromTimeoutException).contains(details)

  def skip: Skipped =
    Skipped(m, e)

/** Trait to model detailed information for failures so that smart differences can be computed
  */
sealed trait Details derives CanEqual
case class FailureDetailsMessages(messages: List[String]) extends Details
case class FailureDetails(actual: String, expected: String) extends Details
case object NoDetails extends Details

case object FromNotImplementedError extends Details
case object FromJUnitAssertionError extends Details
case object FromTimeoutException extends Details

// This error type can be found in a library like JMock
case object FromExpectationError extends Details

/** This class represents an exception occurring during an execution.
  */
case class Error(m: String, t: Throwable) extends Result(s"${t.getClass.getName}: $m") with ResultStackTrace { outer =>
  type SelfType = Error

  /** @return an exception created from the message and the stackTraceElements */
  def exception = t
  def stackTrace = t.getFullStackTrace

  override def equals(o: Any) =
    o.asInstanceOf[Matchable] match
      case Error(m2, t2) => m == m2 && t.getMessage.notNull == t2.getMessage.notNull
      case _             => false

  def setExpectationsNb(n: Int) =
    new Error(m, t) {
      override def expectationsNb: Int = n
    }

  def mute = copy(m = "")

  override def hashCode = m.hashCode
  override def isError: Boolean = true
}

/** This object allows to create an Error from an exception
  */
case object Error:
  def apply(t: Throwable) = new Error(t.getMessage.notNull, t)
  def apply(m: String = "") = new Error(m, new Exception(m))

/** Pending result
  * @see
  *   Result for description
  */
case class Pending(m: String = "") extends Result(m) { outer =>
  type SelfType = Pending

  def mute = Pending()
  def setExpectationsNb(n: Int) =
    new Pending(m) {
      override def expectationsNb: Int = n
    }

  override def isPending: Boolean = true
}

/** Skipped result
  * @see
  *   Result for description
  */
case class Skipped(m: String = "", e: String = "") extends Result(m, e) { outer =>
  type SelfType = Skipped

  def mute = Skipped()
  def setExpectationsNb(n: Int) =
    new Skipped(m) {
      override def expectationsNb: Int = n
    }

  override def isSkipped: Boolean = true
}

/** This result allows to embed additional data with a given result for further display
  *
  * Is is used to provide a way to display properly the data tables in the HtmlExporter
  */
case class DecoratedResult[+T](decorator: T, result: Result) extends Result(result.message, result.expected) { outer =>
  type SelfType = Result

  def mute = DecoratedResult(decorator, result.mute)
  def setExpectationsNb(n: Int) =
    new DecoratedResult(decorator, result) {
      override def expectationsNb: Int = n
    }

  override def isSuccess: Boolean = result.isSuccess
  override def isError: Boolean = result.isError
  override def isSkipped: Boolean = result.isSkipped
  override def isPending: Boolean = result.isPending
  override def isFailure: Boolean = result.isFailure

  /** use another decorator */
  def decorate[S](otherDecorator: S) = DecoratedResult(otherDecorator, result)
}
