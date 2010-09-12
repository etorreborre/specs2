package org.specs2
package specification
import matcher.AnyMatchers._
import matcher._
import execute._

trait ShouldExpectations {
  implicit def theValue[T](t: =>T): ShouldExpectable[T] = new ShouldExpectable(t)
}
trait MustExpectations {
  implicit def theValue[T](t: =>T): MustExpectable[T] = new MustExpectable(t)
}
class MustExpectable[T](t: =>T) {
  def must(m: => Matcher[T]) = m.apply(t) 
  def must_==(other: =>T) = must(new BeMatcher(other))
}
class ShouldExpectable[T](t: =>T) {
  def should(m: => Matcher[T]) = m.apply(t) 
  def should_==(other: =>T) = {
	val (a, b) = (t, other)
	if (a == b) Success(q(a) + " is equal to " + q(b)) 
	else Failure(q(a) + " is not equal to " + q(b))
  }
  def q(a: Any) = "'"+a+"'"
}
