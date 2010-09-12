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
trait Expectable[T] {
  protected[specs2] val description: Option[String] = None
  /** @return the description of the matched value, quoted. */
  protected def d(value: Any) = description match {
    case None => q(value)
    case Some(desc: String) => desc + " " + q(value)
  }
  /** @return the description of the matched value, unquoted. */
  protected def dUnquoted(value: Any) = description match {
    case None => unq(value)
    case Some(desc) => desc + " " + unq(value)  
  }
}
class MustExpectable[T](t: =>T) extends Expectable[T] {
  def aka(desc: String) = new MustExpectable(t) {
	override val description = Some(desc)
  } 
  def must(m: => Matcher[T]) = m.apply(t)(d(_)) 
  def must_==(other: =>T) = must(new BeMatcher(other))
}
class ShouldExpectable[T](t: =>T) extends Expectable[T]{
  def aka(desc: String) = new MustExpectable(t) {
	override val description = Some(desc)
  } 
  def should(m: => Matcher[T]) = m.apply(t)(d(_))
  def should_==(other: =>T) = {
	val (a, b) = (t, other)
	if (a == b) Success(q(a) + " is equal to " + q(b)) 
	else Failure(q(a) + " is not equal to " + q(b))
  }
  def q(a: Any) = "'"+a+"'"
}
