package org.specs2
package matcher

import org.specs2.internal.scalaz._
import reflect.Anyx._
import execute._
import text.Quote._
import text.NotNullStrings._
import Expectable._
/**
 * The Expectable class models anything which can be checked by applying a Matcher
 * 
 * It stores a value which is only evaluated when necessary and an optional additional description for that value.
 * 
 * The Expectable object is responsible for creating its own description, based on the value toString method and
 * and an additional description.
 *
 */
class Expectable[+T] private[specs2] (t: () => T) { outer =>
  /** the value is only evaluated if necessary */
  lazy val value = t()
  
  /**
   * optional additional description: it is a function which takes value.toString and returns a String
   */
  private[specs2] val desc: Option[String => String] = None
  /**
   * optional user description for the value
   */
  private[specs2] val showValueAs: Option[() => String] = None
  /**
   * @return a description of the value provided by the user
   *         a combination of the value show by specs2 and an optional description
   */
  def description    = describe(value)
  /**
   * @return a description of any value with the custom description
   */
  def describe(v: Any) = showValueAs.map(_()).getOrElse(d(v, desc))
  /** @return the optional description function */
  def optionalDescription: Option[String => String] = desc

  /**
   * apply a matcher on the value and return a MatchResult which can later on be transformed to a simple Result
   */
  def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = {
    if (m == null) throw new IllegalArgumentException("You cannot use a null matcher on "+description)
    check(m.apply(this))
  }
  /** additional checks can be done on the result, such as throwing an exception */
  def check[S >: T](result: MatchResult[S]) = result

  /** evaluate the value and return the same expectable */
  def evaluate = { value; this }

  /** evaluate the value once and return an expectable with the same expression, ready to be evaluated again */
  def evaluateOnce = Expectable(t(), desc, showValueAs)
  /**
   * apply a function to the expectable value
   */
  def map[S](f: T => S): Expectable[S] = Expectable(f(value), desc)
  /**
   * apply a function to the value
   */
  def flatMap[S](f: T => Expectable[S]): Expectable[S] = f(value)
  /**
   * change the expectable value
   */
  def map[S](other: S): Expectable[S] = map(t => other)
  /**
   * apply a function to the description function
   */
  def mapDescription(d: Option[String => String]): Expectable[T] = Expectable(value, d)
  def mapDescription(d: String => String): Expectable[T] = mapDescription(Some(d))
  def mapDescription(d: String): Expectable[T] = mapDescription((_:String) => d)
}

/**
 * Factory methods for creating Expectables
 */
object Expectable {
  /** @return an Expectable with t as a value */
  private[specs2] def apply[T](t: =>T) = new Expectable(() => t)
  /** @return an Expectable with t as a value, and a constant string for its description */
  private[specs2] def apply[T](t: =>T, d1: =>String) = new Expectable(() => t) {
    override val desc: Option[String => String] = Some(aliasDisplay(d1))
  }
  private[specs2] def aliasDisplay(d1: =>String) = (s: String) => d1 + (if (!s.isEmpty && !Seq("true", "false").contains(s)) " " + q(s) else "")
  /** @return an Expectable with t as a value, and a description function */
  private[specs2] def apply[T](t: =>T, d1: Option[String => String]) = new Expectable(() => t) {
    override val desc: Option[String => String] = d1
  }
  /** @return an Expectable with t as a value, and a description function */
  private[specs2] def apply[T](t: =>T, d1: Option[String => String], show: Option[() => String]) = new Expectable(() => t) {
    override val desc: Option[String => String] = d1
    override val showValueAs: Option[() => String] = show
  }
  /** @return an Expectable with t as a value, and string showing the element t */
  private[specs2] def createWithShowAs[T](t: =>T, show: =>String) = new Expectable(() => t) {
    override val showValueAs: Option[() => String] = Some(() => show)
  }

  /** Expectable is a Functor and can use the fmap function to modify its value */
  implicit val ExpectableFunctor: Functor[Expectable] = new Functor[Expectable] {
    def fmap[A, B](r: Expectable[A], f: A => B) = r.map(f)
  }

  /** @return the description of the matched value, quoted. */
  private[specs2] def d(value: =>Any, desc: Option[String => String]) = {
    desc match {
      case None => value match {
        case b: Boolean   => "the value"
        case _            => q(value)
      }
      case Some(de)       => de(value.notNull)
    }
  }

  /** @return the description of the matched value, unquoted. */
  private[specs2] def dUnquoted[T](value: T, desc: Option[String => String]) = desc match {
    case None     => unq(value)
    case Some(de) => de(unq(value))
  }


}
