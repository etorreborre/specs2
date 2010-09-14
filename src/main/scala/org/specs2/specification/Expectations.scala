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
  def aka(alias: String) = new MustExpectable(t) {
	override val description = Some(alias)
  } 
  def must(b: BeHave): Expectable[T] = this
  def must(m: =>Matcher[T]) = applyMatcher(m)
  def must_==(other: =>T) = must(new BeEqualToMatcher(other))
}
class ShouldExpectable[T](t: =>T) extends Expectable[T](t ){
  def aka(alias: String) = new MustExpectable(t) {
	override val description = Some(alias)
  } 
  def should(b: BeHave): Expectable[T] = this
  def should(m: =>Matcher[T]) = applyMatcher(m)
  def should_==(other: =>T) = should(new BeEqualToMatcher(other))
}
class BeHave
object be extends BeHave
