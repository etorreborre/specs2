package org.specs2
package matcher

import scalaz._
import execute._
import AnyMatchers._

/**
 * The Expectable class models anything which can be checked by applying a Matcher
 * 
 * It stores a value which is only evaluated when necessary and an optional additional
 * description for that value.
 * 
 * The Expectable object is responsible for creating its own description, based on the
 * value toString method and the additional description.
 *
 */
class Expectable[+T](private val t: () => T) { outer =>
  
  /** the value is only evaluated if necessary */
  lazy val value = t()
  
  def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = {
	  m.apply(this) 
  }
  /**
   * @return
   */
  def description = d(value)
  protected val desc: Option[String] = None
  /** @return the description of the matched value, quoted. */
  protected def d[T](value: =>T) = desc  match {
    case None => if (isBoolean(value)) "the value" else q(value)
    case Some(de: String) => de + (if (!value.toString.isEmpty && !isBoolean(value)) " " + q(value) else "")
  }
  /** @return the description of the matched value, unquoted. */
  protected def dUnquoted[T](value: T) = desc match {
    case None => unq(value)
    case Some(de) => de + " " + unq(value)  
  }
}

object Expectable {
  def apply[T](t: =>T) = new Expectable(() => t)
  def apply[T](t: =>T, d1: String) = new Expectable(() => t) {
    override val desc: Option[String] = Some(d1)
  }
  def apply[T](t: =>T, d1: Option[String]) = new Expectable(() => t) {
    override val desc: Option[String] = d1
  }
  
  implicit def ExpectableFunctor[T](e: Expectable[T]): Functor[Expectable] = new Functor[Expectable] {
    def fmap[A, B](r: Expectable[A], f: A => B) = Expectable(f(r.value), r.desc)
  }
}