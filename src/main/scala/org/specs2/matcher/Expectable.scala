package org.specs2
package matcher
import AnyMatchers._
import execute._

class Expectable[+T](t: =>T) {
  protected val desc: Option[String] = None
  lazy val value = t
  
  def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = {
	m.apply(this) 
  }
  
  def description = d(value)
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
