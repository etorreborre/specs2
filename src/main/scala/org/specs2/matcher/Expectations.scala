package org.specs2
package matcher

import execute._
import AnyMatchers._

trait Expectations {
  type E[_] <: Expectable[_]
  implicit def theValue[T](t: =>T): E[T]
}
trait MustExpectations extends Expectations {
  type E[_] = MustExpectable[_]
  implicit def theValue[T](t: =>T): MustExpectable[T] = new MustExpectable(t)
  implicit def theBlock(t: =>Nothing): MustExpectable[Nothing] = new MustExpectable(t)
}
trait ShouldExpectations extends Expectations {
  type E[_] = ShouldExpectable[_]
  implicit def theValue[T](t: =>T): ShouldExpectable[T] = new ShouldExpectable(t)
}
