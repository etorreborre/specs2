package org.specs2
package matcher


/**
 * This trait adds some implicits to create expectations with the `===` sign
 */
trait TypedEqualExpectations { this: ExpectationsCreation =>
  /**
   * A value can be tested against another with the === operator.
   * It is equivalent to writing a must_== b
   */
  implicit def typedEqualExpectation[T](t: =>T) = new TypedEqualExpectation(t)
  class TypedEqualExpectation[T](t: =>T) {
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

object TypedEqualExpectations extends ExpectationsCreation
/**
 * This trait can be used to suppress the TypedEqualExpectations implicits
 */
trait NoTypedEqualExpectations extends TypedEqualExpectations { this: ExpectationsCreation =>
  override def typedEqualExpectation[T](t: =>T) = super.typedEqualExpectation(t)
}