package org.specs2
package matcher
import AnyMatchers._
import execute._

abstract class Expectable[T](t: =>T) {
  protected val desc: Option[String] = None
  
  def applyMatcher(m: Matcher[T]): MatchResult[T] = {
	lazy val value = t
	m.apply(t)(this) 
  }
  
  def description = d(t)
  /** @return the description of the matched value, quoted. */
  protected def d[T](value: =>T) = desc  match {
    case None => q(value)
    case Some(de: String) => de + (if (!value.toString.isEmpty && !isBoolean(value)) " " + q(value) else "")
  }
  /** @return the description of the matched value, unquoted. */
  protected def dUnquoted[T](value: T) = desc match {
    case None => unq(value)
    case Some(de) => de + " " + unq(value)  
  }
}
