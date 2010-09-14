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
class MustExpectable[T](t: =>T) extends Expectable[T](t) {
  def aka(alias: String) = new MustExpectable(t) {
	override protected val desc = Some(alias)
  } 
  def mustNot(m: =>Matcher[T]) = applyMatcher(m.not)
  def must(m: =>Matcher[T]) = applyMatcher(m)
  def must_==(other: =>T) = must(new BeEqualToMatcher(other))
}
class ShouldExpectable[T](t: =>T) extends Expectable[T](t ){
  def aka(alias: String) = new MustExpectable(t) {
	override protected val desc = Some(alias)
  } 
  def should(m: =>Matcher[T]) = applyMatcher(m)
  def shouldNot(m: =>Matcher[T]) = applyMatcher(m.not)
  def should_==(other: =>T) = should(new BeEqualToMatcher(other))
}
