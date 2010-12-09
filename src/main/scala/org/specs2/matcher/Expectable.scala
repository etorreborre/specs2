package org.specs2
package matcher

import scalaz._
import reflect.Anyx._
import execute._
import text.Quote._

/**
 * The Expectable class models anything which can be checked by applying a Matcher
 * 
 * It stores a value which is only evaluated when necessary and an optional additional
 * description for that value
 * 
 * The Expectable object is responsible for creating its own description, based on the
 * value toString method and the additional description.
 *
 */
class Expectable[+T] protected (private[specs2] val t: () => T) { outer =>
  /** the value is only evaluated if necessary */
  lazy val value = t()
  
  /** 
   * apply a matcher on the value an return a MatchResult
   * which can later on be transformed to a simple Result 
   */
  def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = {
	  m.apply(this) 
  }
  /**
   * @return a description of the value
   */
  def description = d(value)

  /** 
   * @return this expectable with its toString method as an alias description
   *         this is useful to preserve the original value when the matcher using
   *         it is adapting the value 
   */
  def aka = Expectable(value, value.toString)
  
  /** @return this expectable with an alias description */
  def aka(alias: String) = Expectable(value, alias)
  
  /** equality matcher on Expectables */
  def ===[S >: T](other: =>S) = applyMatcher(new BeEqualTo(other))
  
  /** optional additional description */
  protected val desc: Option[String] = None
  /** evaluate the value once and return the same expectable */
  private[specs2] def evaluate = Expectable(t(), desc)
  /** @return the description of the matched value, quoted. */
  protected def d[T](value: =>T) = desc  match {
    case None => if (value.isBoolean) "the value" else q(value)
    case Some(de: String) => de + (if (!value.toString.isEmpty && !value.isBoolean) " " + q(value) else "")
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
  
  implicit val ExpectableFunctor: Functor[Expectable] = new Functor[Expectable] {
    def fmap[A, B](r: Expectable[A], f: A => B) = Expectable(f(r.value), r.desc)
  }
}