package org.specs2
package matcher

import org.specs2.execute.{Result, StandardResults}


/**
 * This trait provides implicit definitions to transform any value into a MustExpectable
 */
trait MustExpectations extends MustExpectations1 with ExpectationsDescription with TypedEqual {
  implicit def akaMust[T](tm: Expectable[T]): MustExpectable[T] = new MustExpectable(() => tm.valueDefinition()) {
    override private[specs2] val desc = tm.desc
    override private[specs2] val showValueAs = tm.showValueAs
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
    override def checkResult(r: Result): Result = checkResultFailure(r)
  }

  implicit def theBlock(t: =>Nothing): MustExpectable[Nothing] = createMustExpectable(t)
}

private[specs2]
trait MustExpectations1 extends MustExpectationsCreation {
  implicit def theValue[T](t: => T): MustExpectable[T] = createMustExpectable(t)
}

private[specs2]
trait MustExpectationsCreation extends ExpectationsCreation {
  protected def createMustExpectable[T](t: =>T) = new MustExpectable(() => t) {
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
    override def checkResult(r: Result): Result = checkResultFailure(r)
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
trait MustThrownExpectations extends MustThrownExpectables with StandardResults with StandardMatchResults

trait MustThrownExpectables extends MustExpectations with MustThrownExpectations1 {
  override implicit def akaMust[T](tm: Expectable[T]): MustExpectable[T] = new MustExpectable(() => tm.valueDefinition()) {
    override private[specs2] val desc = tm.desc
    override private[specs2] val showValueAs = tm.showValueAs
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
    override def checkResult(r: Result): Result = checkResultFailure(r)
  }
}


private[specs2]
trait MustThrownExpectations1 extends MustExpectations1 with MustThrownExpectationsCreation with StandardResults with StandardMatchResults

trait MustThrownExpectationsCreation extends ThrownExpectationsCreation with MustExpectationsCreation {
  override protected def createMustExpectable[T](t: =>T) = new MustExpectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
  }
}

object MustThrownExpectations extends MustThrownExpectations


