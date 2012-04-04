package org.specs2
package matcher

/**
 * This trait provides implicit definitions to transform any value into a ShouldExpectable
 */
trait ShouldExpectations extends Expectations {
  implicit def akaShould[T](tm: Expectable[T]) = new ShouldExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
    override private[specs2] val showValueAs = tm.showValueAs
  }
  implicit def thisValue[T](t: =>T): ShouldExpectable[T] = createShouldExpectable(t)
  implicit def thisBlock(t: =>Nothing): ShouldExpectable[Nothing] = createShouldExpectable(t)

  protected def createShouldExpectable[T](t: =>T) = new ShouldExpectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(m.apply(this))
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
  override implicit def akaShould[T](tm: Expectable[T]) = new ShouldExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
    override private[specs2] val showValueAs = tm.showValueAs
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(super.applyMatcher(m))
  }
  override protected def createShouldExpectable[T](t: =>T) = new ShouldExpectable(() => t) {
    override def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = checkFailure(super.applyMatcher(m))
  }
}
object ShouldThrownExpectations extends ShouldThrownExpectations
