package org.specs2
package matcher

import execute._
import AnyMatchers._

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
    def aka = Expectable(value, value.toString)

    /** @return an expectable with an alias description */
    def aka(alias: String) = Expectable(value, alias)
  }
  implicit def canEqual[T](t: =>T) = new CanEqual(t)
  class CanEqual[T](t: =>T) {
    /** equality matcher on Expectables */
    def ===[S >: T](other: =>S) = Expectable(t).applyMatcher(new BeEqualTo(other))
  }

}
/**
 * This trait provides implicit definitions to transform any value into a MustExpectable
 */
trait MustExpectations extends Expectations {
  implicit def akaMust[T](tm: Expectable[T]) = new MustExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
  }

  implicit def theValue[T](t: =>T): MustExpectable[T] = MustExpectable(t)
  implicit def theBlock(t: =>Nothing): MustExpectable[Nothing] = MustExpectable(t)
}
object MustExpectations extends MustExpectations
/**
 * This trait provides implicit definitions to transform any value into a ShouldExpectable
 */
trait ShouldExpectations extends Expectations {
  implicit def akaShould[T](tm: Expectable[T]) = new ShouldExpectable(() => tm.value) {
    override private[specs2] val desc = tm.desc
  }
  implicit def thisValue[T](t: =>T): ShouldExpectable[T] = ShouldExpectable(t)
  implicit def thisBlock(t: =>Nothing): ShouldExpectable[Nothing] = ShouldExpectable(t)
}
object ShouldExpectations extends ShouldExpectations
