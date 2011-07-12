package org.specs2
package matcher

/**
 * This trait provides implicit definitions to transform any value into a ShouldExpectable
 */
trait ShouldExpectations extends Expectations {
  implicit def akaShould[T](tm: Expectable[T]) = new ShouldExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
  }
  implicit def thisValue[T](t: =>T): ShouldExpectable[T] = createShouldExpectable(t)
  implicit def thisBlock(t: =>Nothing): ShouldExpectable[Nothing] = createShouldExpectable(t)

  protected def createShouldExpectable[T](t: =>T) = new ShouldExpectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(m.apply(this))
  }
}
object ShouldExpectations extends ShouldExpectations

/**
 * This trait provides implicit definitions to transform any value into a ShouldExpectable, throwing exceptions when
 * a match fails
 */
trait ShouldThrownExpectations extends ThrownExpectations with ShouldExpectations {
  override implicit def akaShould[T](tm: Expectable[T]) = new ShouldExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(super.applyMatcher(m))
  }
  override protected def createShouldExpectable[T](t: =>T) = new ShouldExpectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(super.applyMatcher(m))
  }
}
object ShouldThrownExpectations extends ShouldThrownExpectations
