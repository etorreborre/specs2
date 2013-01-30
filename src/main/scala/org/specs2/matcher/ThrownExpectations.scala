package org.specs2
package matcher

import execute._
import execute.Skipped
import execute.Pending
import execute.Failure
import scala.Some
import execute.PendingException
import execute.SkipException
import execute.FailureException

/**
 * Thrown expectations will throw a FailureException if a match fails
 *
 * This trait can be extended to be used in another framework like ScalaTest:
 *
 *   trait ScalaTestExpectations extends ThrownExpectations {
 *     override protected def checkFailure[T](m: =>MatchResult[T]) = {
 *       m match {
 *         case f @ MatchFailure(ok, ko, _, _) => throw new TestFailedException(f.message, f.exception, 0)
 *         case _ => ()
 *       }
 *       m
 *     }
 *   }
 */
trait ThrownExpectations extends Expectations with StandardResults {
  override def createExpectable[T](t: =>T, alias: Option[String => String]): Expectable[T] =
    new Expectable(() => t) {
      override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(super.applyMatcher(m))
      override val desc = alias
      override def map[S](f: T => S): Expectable[S] = createExpectable(f(value), desc)
      override def mapDescription(d: Option[String => String]): Expectable[T] = createExpectable(value, d)
      override def evaluateOnce = {
        val v = t
        createExpectable(t, desc)
      }
    }

  override def createExpectableWithShowAs[T](t: =>T, show: =>String): Expectable[T] =
    new Expectable(() => t) {
      override val showValueAs = Some(() => show)
      override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(super.applyMatcher(m))
      override def map[S](f: T => S): Expectable[S] = createExpectableWithShowAs(f(value), show)
      override def mapDescription(d: Option[String => String]): Expectable[T] = createExpectable(value, d)
      override def evaluateOnce = {
        val (v, s) = (value, show)
        createExpectableWithShowAs(v, s)
      }
    }

  override protected def checkResultFailure(r: Result) = {
    r match {
      case f @ Failure(_,_,_,_)     => throw new FailureException(f)
      case s @ Skipped(_,_)         => throw new SkipException(s)
      case s @ Pending(_)           => throw new PendingException(s)
      case e @ Error(_,_)           => throw new ErrorException(e)
      case d @ DecoratedResult(_,_) => throw new DecoratedResultException(d)
      case _                        => ()
    }
    r
  }
  /** this method can be overriden to throw exceptions when checking the match result */
  override protected def checkMatchResultFailure[T](m: MatchResult[T]) = {
    m match {
      case f @ MatchFailure(_,_,_,_) => throw new MatchFailureException(f)
      case s @ MatchSkip(_,_)        => throw new MatchSkipException(s)
      case p @ MatchPending(_,_)     => throw new MatchPendingException(p)
      case _                         => ()
    }
    m
  }

  override def failure: Failure = throw new FailureException(super.failure)
  override def skipped: Skipped = throw new SkipException(super.skipped)
  override def pending: Pending = throw new PendingException(super.pending)
  override def anError: Error = throw new ErrorException(super.anError)

  protected def failure(m: String): Nothing = failure(Failure(m))
  protected def failure(f: Failure): Nothing = throw new FailureException(f)
  protected def skipped(m: String): Nothing = skipped(Skipped(m))
  protected def skipped(s: Skipped): Nothing = throw new SkipException(s)
  protected def pending(m: String): Nothing = pending(Pending(m))
  protected def pending(s: Pending): Nothing = throw new PendingException(s)
}
private [specs2]
object ThrownExpectations extends ThrownExpectations

/**
 * This trait can be used to cancel the effect of thrown expectations.
 *
 * For example it can be mixed-in a mutable.Specification so that no exception is thrown on failure
 */
trait NoThrownExpectations extends Expectations {
  override protected def checkResultFailure(r: Result) = r
  override protected def checkMatchResultFailure[T](m: MatchResult[T]): MatchResult[T] = m
}

/**
 * This trait can be used to integrate failures and skip messages into specs2
 */
trait ThrownMessages { this: ThrownExpectations =>
  def fail(m: String): Nothing = failure(m)
  def skip(m: String): Nothing = skipped(m)
}

/** this class allows to throw a match failure result in an Exception */
class MatchFailureException[T](val failure: MatchFailure[T]) extends FailureException(failure.toResult) with MatchResultException[T] {
  lazy val matchResult = failure

  override def getMessage = f.message
  override def getCause = f.exception
  override def getStackTrace = f.exception.getStackTrace
}
object MatchFailureException {
  def unapply[T](m: MatchFailureException[T]): Option[MatchFailure[T]] = Some(m.failure)
}
/** this class allows to throw a skipped match result in an Exception */
class MatchSkipException[T](val s: MatchSkip[T]) extends SkipException(s.toResult) with MatchResultException[T] {
  lazy val matchResult = s
}
object MatchSkipException {
  def unapply[T](m: MatchSkipException[T]): Option[MatchSkip[T]] = Some(m.s)
}

/** this class allows to throw a pending result in an Exception */
class MatchPendingException[T](val p: MatchPending[T]) extends PendingException(p.toResult) with MatchResultException[T] {
  lazy val matchResult = p
}
object MatchPendingException {
  def unapply[T](m: MatchPendingException[T]): Option[MatchPending[T]] = Some(m.p)
}

trait MatchResultException[T] {
  def matchResult: MatchResult[T]
}
