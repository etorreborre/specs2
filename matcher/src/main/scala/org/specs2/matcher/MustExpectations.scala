package org.specs2
package matcher

import org.specs2.execute.Result


/**
 * This trait provides implicit definitions to transform any value into a MustExpectable
 */
trait MustExpectations extends MustExpectations1 {
  implicit def akaMust[T](tm: Expectable[T]) = new MustExpectable(() => tm.valueDefinition()) {
    override private[specs2] val desc = tm.desc
    override private[specs2] val showValueAs = tm.showValueAs
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
    override def checkResult(r: Result): Result = checkResultFailure(r)
  }

  implicit def theBlock(t: =>Nothing): MustExpectable[Nothing] = createMustExpectable(t)
}
trait MustExpectations1 extends MustExpectations0 {
  implicit def theValue[T](t: => T): MustExpectable[T] = createMustExpectable(t)
}

trait MustExpectations0 extends Expectations0 {
  protected def createMustExpectable[T](t: =>T) = new MustExpectable(() => t) {
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
  }
}

/**
 * This trait can be used to remove aka and must methods on any value
 */
trait NoMustExpectations extends MustExpectations {
  override def akaMust[T](tm: Expectable[T]) = super.akaMust(tm)
  override def theValue[T](t: =>T): MustExpectable[T] = super.theValue(t)
  override def theBlock(t: =>Nothing): MustExpectable[Nothing] = super.theBlock(t)
}

object MustExpectations extends MustExpectations

/**
 * This trait provides implicit definitions to transform any value into a MustExpectable, throwing exceptions when
 * a match fails
 */
trait MustThrownExpectations extends MustExpectations with MustThrownExpectations1 {
  override implicit def akaMust[T](tm: Expectable[T]) = new MustExpectable(() => tm.valueDefinition()) {
    override private[specs2] val desc = tm.desc
    override private[specs2] val showValueAs = tm.showValueAs
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
    override def checkResult(r: Result): Result = checkResultFailure(r)
  }
}

trait MustThrownExpectations1 extends MustExpectations1 with MustThrownExpectations0

trait MustThrownExpectations0 extends ThrownExpectations with MustExpectations0 {
  override protected def createMustExpectable[T](t: =>T) = new MustExpectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
  }
}

object MustThrownExpectations extends MustThrownExpectations


