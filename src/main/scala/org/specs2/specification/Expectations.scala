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
class MustExpectable[T](t: =>T) extends Expectable[T](t) {
  def aka(desc: String) = new MustExpectable(t) {
	override val description = Some(desc)
  } 
  def must(m: =>Matcher[T]) = applyMatcher(m)
  def must_==(other: =>T) = must(new BeEqualToMatcher(other))
}
class ShouldExpectable[T](t: =>T) extends Expectable[T](t ){
  def aka(desc: String) = new MustExpectable(t) {
	override val description = Some(desc)
  } 
  def should(m: =>Matcher[T]) = applyMatcher(m)
  def should_==(other: =>T) = should(new BeEqualToMatcher(other))
}
