package org.specs2
package matcher
import AnyMatchers._

abstract class Expectable[T](t: =>T) {
  protected[specs2] val description: Option[String] = None
  protected def applyMatcher(m: Matcher[T]) = {
	lazy val value = t
	m.apply(t)(this) 
  }
  def desc = d(t)
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
