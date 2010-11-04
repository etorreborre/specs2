package org.specs2
package matcher

import execute._
import AnyMatchers._

/**
 * This trait provides implicit definitions to transform any value into an Expectable
 */
trait Expectations {
  type E[_] <: Expectable[_]
  implicit def theValue[T](t: =>T): E[T]
}
/**
 * This trait provides implicit definitions to transform any value into a MustExpectable
 */
trait MustExpectations extends Expectations {
  type E[_] = MustExpectable[_]
  implicit def theValue[T](t: =>T): MustExpectable[T] = MustExpectable(t)
  implicit def theBlock(t: =>Nothing): MustExpectable[Nothing] = MustExpectable(t)
}
/**
 * This trait provides implicit definitions to transform any value into a ShouldExpectable
 */
trait ShouldExpectations extends Expectations {
  type E[_] = ShouldExpectable[_]
  implicit def theValue[T](t: =>T): ShouldExpectable[T] = ShouldExpectable(t)
  implicit def theBlock(t: =>Nothing): ShouldExpectable[Nothing] = ShouldExpectable(t)
}
