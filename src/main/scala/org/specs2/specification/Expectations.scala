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
abstract class Expectable[T](t: =>T) {
  protected[specs2] val description: Option[String] = None
  protected def applyMatcher(m: Matcher[T]) = {
	lazy val value = t
	m.apply(value)(d(value)) 
  }
  /** @return the description of the matched value, quoted. */
  protected def d[T](value: T) = description  match {
    case None => q(value)
    case Some(desc: String) => desc + (if (!value.toString.isEmpty && !isBoolean(value)) " " + q(value) else "")
  }
  /** @return the description of the matched value, unquoted. */
  protected def dUnquoted[T](value: T) = description match {
    case None => unq(value)
    case Some(desc) => desc + " " + unq(value)  
  }
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
