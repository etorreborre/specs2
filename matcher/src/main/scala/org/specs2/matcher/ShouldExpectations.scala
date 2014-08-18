package org.specs2
package matcher

import org.specs2.execute.Result


/**
 * This trait provides implicit definitions to transform any value into a ShouldExpectable
 */
trait ShouldExpectations extends Expectations {
  implicit def akaShould[T](tm: Expectable[T]) = new ShouldExpectable(() => tm.valueDefinition()) {
    override private[specs2] val desc = tm.desc
    override private[specs2] val showValueAs = tm.showValueAs
    // overriding this method is necessary to include the ThrownExpectation trait into the stacktrace of the created match result
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
    override def checkResult(r: Result): Result = checkResultFailure(r)
  }
  implicit def thisValue[T](t: =>T): ShouldExpectable[T] = createShouldExpectable(t)
  implicit def thisBlock(t: =>Nothing): ShouldExpectable[Nothing] = createShouldExpectable(t)

  protected def createShouldExpectable[T](t: =>T) = new ShouldExpectable(() => t) {
    // overriding this method is necessary to include the ThrownExpectation trait into the stacktrace of the created match result
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
  }
}
/**
 * This trait can be used to remove aka and should methods on any value
 */
trait NoShouldExpectations extends ShouldExpectations {
  override def akaShould[T](tm: Expectable[T]) = super.akaShould(tm)
  override def thisValue[T](t: =>T): ShouldExpectable[T] = super.thisValue(t)
  override def thisBlock(t: =>Nothing): ShouldExpectable[Nothing] = super.thisBlock(t)
}

object ShouldExpectations extends ShouldExpectations

/**
 * This trait provides implicit definitions to transform any value into a ShouldExpectable, throwing exceptions when
 * a match fails
 */
trait ShouldThrownExpectations extends ThrownExpectations with ShouldExpectations {
  override implicit def akaShould[T](tm: Expectable[T]) = new ShouldExpectable(() => tm.valueDefinition()) {
    override private[specs2] val desc = tm.desc
    override private[specs2] val showValueAs = tm.showValueAs
    // overriding this method is necessary to include the ThrownExpectation trait into the stacktrace of the created match result
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
    override def checkResult(r: Result): Result = checkResultFailure(r)
  }
  override protected def createShouldExpectable[T](t: =>T) = new ShouldExpectable(() => t) {
    // overriding this method is necessary to include the ThrownExpectation trait into the stacktrace of the created match result
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = super.applyMatcher(m)
    override def check[S >: T](r: MatchResult[S]): MatchResult[S] = checkFailure(r)
  }
}
object ShouldThrownExpectations extends ShouldThrownExpectations
