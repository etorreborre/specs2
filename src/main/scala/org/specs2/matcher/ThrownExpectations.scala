package org.specs2
package matcher

import execute.{FailureException, SkipException, Pending, PendingException, Failure, Skipped, Result}

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
trait ThrownExpectations extends Expectations {
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
      case f @ Failure(_,_,_,_) => throw new FailureException(f)
      case s @ Skipped(_,_)     => throw new SkipException(s)
      case s @ Pending(_)       => throw new PendingException(s)
      case _                    => ()
    }
    r
  }
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
}

/**
 * This trait can be used to integrate failures and skip messages into specs2
 */
trait ThrownMessages { this: ThrownExpectations =>
  def fail(m: String): Nothing = failure(m)
  def skip(m: String): Nothing = skipped(m)
}