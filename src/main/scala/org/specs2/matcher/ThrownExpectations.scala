package org.specs2
package matcher

import execute.Failure

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
  override protected def createExpectable[T](t: =>T) = new Expectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(m.apply(this))
  }
  override protected def createExpectable[T](t: =>T, alias: String) = new Expectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(m.apply(this))
    override val desc = Some((_:String) => alias)
  }
  override protected def createExpectable[T](t: =>T, alias: String => String) = new Expectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(m.apply(this))
    override val desc = Some(alias)
  }
  protected def checkFailure[T](m: =>MatchResult[T]) = {
    m match {
      case f @ MatchFailure(ok, ko, _, _) => throw new FailureException(f.toResult)
      case _ => ()
    }
    m
  }
}
case class FailureException(f: Failure) extends Exception
