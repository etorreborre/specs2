package org.specs2
package matcher

import scalaz._
import reflect.Anyx._
import execute._
import text.Quote._

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
  /** @return a description of the value */
  def description = d(value)
  /** @return the optional description function */
  def optionalDescription: Option[String => String] = desc

  /**
   * apply a matcher on the value and return a MatchResult which can later on be transformed to a simple Result
   */
  def applyMatcher[S >: T](m: =>Matcher[S]): MatchResult[S] = m.apply(this)

  /** evaluate the value once and return the same expectable */
  protected[specs2] def evaluate = Expectable(t(), desc)

  /** @return the description of the matched value, quoted. */
  protected def d[T](value: =>T) = {
    val valueAsString = value match {
      case it: Iterable[_] => it.mkString(",")
      case _               => value.toString
    }
    desc match {
      case None => value match {
        case b: Boolean   => "the value"
        case _            => q(valueAsString)
      }
      case Some(de) => de(valueAsString)
    }
  }
  /** @return the description of the matched value, unquoted. */
  protected def dUnquoted[T](value: T) = desc match {
    case None => unq(value)
    case Some(de) => de(unq(value))
  }

  def map[S](f: T => S): Expectable[S] = Expectable(f(value), desc)
  def map[S](other: S): Expectable[S] = map(t => other)
}

/**
 * Factory methods for creating Expectables
 */
object Expectable {
  /** @return an Expectable with t as a value */
  private[specs2] def apply[T](t: =>T) = new Expectable(() => t)
  /** @return an Expectable with t as a value, and a constant string for its description */
  private[specs2] def apply[T](t: =>T, d1: String) = new Expectable(() => t) {
    override val desc: Option[String => String] = Some(aliasDisplay(d1))
  }
  private[specs2] def aliasDisplay(d1: String) = (s: String) => d1 + (if (!s.isEmpty && !Seq("true", "false").contains(s)) " " + q(s) else "")
  /** @return an Expectable with t as a value, and a description function */
  private[specs2] def apply[T](t: =>T, d1: Option[String => String]) = new Expectable(() => t) {
    override val desc: Option[String => String] = d1
  }
  
  /** Expectable is a Functor and can use the fmap function to modify its value */
  implicit val ExpectableFunctor: Functor[Expectable] = new Functor[Expectable] {
    def fmap[A, B](r: Expectable[A], f: A => B) = r.map(f)
  }
}