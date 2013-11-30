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
trait ThrownExpectations extends Expectations with StandardResults with StandardMatchResults with ScopedExpectations {
  override def createExpectable[T](t: =>T, alias: Option[String => String]): Expectable[T] =
    new Expectable(() => t) {
      // overriding this method is necessary to include the ThrownExpectation trait into the stacktrace of the created match result
      override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
      override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
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
      // overriding this method is necessary to include the ThrownExpectation trait into the stacktrace of the created match result
      override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
      override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
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
  override def todo: Pending = throw new PendingException(super.todo)
  override def pending: Pending = throw new PendingException(super.pending)
  override def anError: Error = throw new ErrorException(super.anError)

  protected def success(m: String): Success = Success(m)
  protected def failure(m: String): Failure = failure(Failure(m))
  protected def failure(f: Failure): Failure = throw new FailureException(f)
  protected def skipped(m: String): Skipped = skipped(Skipped(m))
  protected def skipped(s: Skipped): Skipped = throw new SkipException(s)
  protected def pending(m: String): Pending = pending(Pending(m))
  protected def pending(s: Pending): Pending = throw new PendingException(s)

  override lazy val ko: MatchFailure[None.type] = throw new MatchFailureException(MatchFailure("ok", "ko", createExpectable(None)))
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
 * This trait represents any Scope that is used to enclose expectations which might be thrown
 */
trait Scope
/**
 * This trait allows to enclose expectations throwing exceptions in a `Scope` trait:
 *
 * "this is a failing test" >> new Scope { 1 must_== 2 }
 */
trait ScopedExpectations {
  /** transform a scope to a success to be able to create traits containing any variables and usable in any Examples */
  implicit def inScope(s: Scope): Success = Success()
  /** typeclass to transform a Scope to a Result */
  implicit def scopeAsResult[S <: Scope]: AsResult[S] = new AsResult[S] {
    def asResult(t: =>S) = inScope(t)
  }
}
trait NoScopedExpectations extends ScopedExpectations {
  override def inScope(s: Scope): Success = super.inScope(s)
  override def scopeAsResult[S <: Scope]: AsResult[S] = super.scopeAsResult
}


/**
 * This trait can be used to integrate failures and skip messages into specs2
 */
trait ThrownMessages { this: ThrownExpectations =>
  def fail(m: String): Nothing = throw new FailureException(Failure(m))
  def skip(m: String): Nothing = throw new SkipException(Skipped(m))
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
