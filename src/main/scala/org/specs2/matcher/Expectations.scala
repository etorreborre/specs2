package org.specs2
package matcher

import execute._
import AnyMatchers._
import junit.framework.{ ComparisonFailure, AssertionFailedError }

/**
 * This trait provides implicit definitions to transform any value into an Expectable
 */
trait Expectations {
  implicit def describe[T](t: =>T): Descriptible[T] = new Descriptible(t)
  class Descriptible[T](value: =>T) {
    /**
     * @return an expectable with its toString method as an alias description
     *         this is useful to preserve the original value when the matcher using
     *         it is adapting the value
     */
    def aka: Expectable[T] = aka(value.toString)

    /** @return an expectable with an alias description */
    def aka(alias: String): Expectable[T] = createExpectable(value, alias)
    /** @return an expectable with an alias description, after the value string */
    def post(alias: String): Expectable[T] = as((_:String) + alias)
    /** @return an expectable with an alias description, after the value string */
    def as(alias: String => String): Expectable[T] = createExpectable(value, alias)
  }
  implicit def canEqual[T](t: =>T) = new CanEqual(t)
  class CanEqual[T](t: =>T) {
    /** equality matcher on Expectables */
    def ===[S >: T](other: =>S) = createExpectable(t).applyMatcher(new BeEqualTo(other))
  }
  protected def createExpectable[T](t: =>T) = Expectable(t)
  protected def createExpectable[T](t: =>T, alias: String) = Expectable(t, alias)
  protected def createExpectable[T](t: =>T, alias: String => String) = Expectable(t, Some(alias))
}
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

trait JUnitExpectations extends ThrownExpectations {
  override protected def checkFailure[T](m: =>MatchResult[T]) = {
    m match {
      case f @ MatchFailure(ok, ko, _, NoDetails()) => throw new AssertionFailedError(ko) {
        override def getStackTrace = f.exception.getStackTrace
        override def getCause = f.exception.getCause
        override def printStackTrace = f.exception.printStackTrace
        override def printStackTrace(w: java.io.PrintStream) = f.exception.printStackTrace(w)
        override def printStackTrace(w: java.io.PrintWriter) = f.exception.printStackTrace(w)
      }
      case f @ MatchFailure(ok, ko, _, FailureDetails(expected, actual)) => throw new ComparisonFailure(ko, expected, actual) {
        override def getStackTrace = f.exception.getStackTrace
        override def getCause = f.exception.getCause
        override def printStackTrace = f.exception.printStackTrace
        override def printStackTrace(w: java.io.PrintStream) = f.exception.printStackTrace(w)
        override def printStackTrace(w: java.io.PrintWriter) = f.exception.printStackTrace(w)
      }
      case _ => ()
    }
    m
  }
}
/**
 * This trait provides implicit definitions to transform any value into a MustExpectable
 */
trait MustExpectations extends Expectations {
  implicit def akaMust[T](tm: Expectable[T]) = new MustExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
  }
  implicit def theValue[T](t: =>T): MustExpectable[T] = createMustExpectable(t)
  implicit def theBlock(t: =>Nothing): MustExpectable[Nothing] = createMustExpectable(t)

  protected def createMustExpectable[T](t: =>T) = MustExpectable(t)
}
object MustExpectations extends MustExpectations

trait MustThrownExpectations extends ThrownExpectations with MustExpectations {
  override implicit def akaMust[T](tm: Expectable[T]) = new MustExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(m.apply(this))
  }
  override protected def createMustExpectable[T](t: =>T) = new MustExpectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(m.apply(this))
  }
}

trait JUnitMustExpectations extends MustThrownExpectations with JUnitExpectations
/**
 * This trait provides implicit definitions to transform any value into a ShouldExpectable
 */
trait ShouldExpectations extends Expectations {
  implicit def akaShould[T](tm: Expectable[T]) = new ShouldExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
  }
  implicit def thisValue[T](t: =>T): ShouldExpectable[T] = createShouldExpectable(t)
  implicit def thisBlock(t: =>Nothing): ShouldExpectable[Nothing] = createShouldExpectable(t)

  protected def createShouldExpectable[T](t: =>T) = ShouldExpectable(t)
}
object ShouldExpectations extends ShouldExpectations

trait ShouldThrownExpectations extends ThrownExpectations with ShouldExpectations {
  override implicit def akaShould[T](tm: Expectable[T]) = new ShouldExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(m.apply(this))
  }
  override protected def createShouldExpectable[T](t: =>T) = new ShouldExpectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(m.apply(this))
  }
}
trait JUnitShouldExpectations extends ShouldThrownExpectations with JUnitExpectations