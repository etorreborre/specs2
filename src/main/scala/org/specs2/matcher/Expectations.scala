package org.specs2
package matcher
import AnyMatchers._
import execute._

trait Expectations
trait MustExpectations extends Expectations {
  implicit def theValue[T](t: =>T): MustExpectable[T] = new MustExpectable(t)
}
trait ShouldExpectations extends Expectations {
  implicit def theValue[T](t: =>T): ShouldExpectable[T] = new ShouldExpectable(t)
}
