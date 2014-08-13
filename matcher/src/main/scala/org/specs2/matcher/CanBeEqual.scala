package org.specs2
package matcher


/**
 * This trait adds some implicits to create expectations with the `===` sign
 */
trait CanBeEqual { this: Expectations =>
  /**
   * A value can be tested against another with the === operator.
   * It is equivalent to writing a must_== b
   */
  implicit def canBeEqual[T](t: =>T) = new CanBeEqualExpectation(t)
  class CanBeEqualExpectation[T](t: =>T) {
    /** equality matcher on Expectables */
    def ===[S >: T](other: =>S) = createExpectable(t).applyMatcher[S](new BeEqualTo(other))
    /** ! equality matcher on Expectables */
    def !==[S >: T](other: =>S) = createExpectable(t).applyMatcher[S](new BeEqualTo(other).not)
    /** typed equality matcher on Expectables */
    def ====(other: =>T) = createExpectable(t).applyMatcher(new BeTypedEqualTo(other))
    /** ! typed equality matcher on Expectables */
    def !===(other: =>T) = createExpectable(t).applyMatcher(new BeTypedEqualTo(other).not)
  }
}

/**
 * This trait can be used to suppress the CanBeEqual implicits
 */
trait NoCanBeEqual extends CanBeEqual { this: Expectations =>
  override def canBeEqual[T](t: =>T) = super.canBeEqual(t)
}