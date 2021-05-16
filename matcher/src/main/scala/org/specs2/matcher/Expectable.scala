package org.specs2
package matcher

import org.specs2.fp.*
import text.Quote.*
import text.NotNullStrings.*
import execute.Result
import Expectable.*

/**
 * The Expectable class models anything which can be checked by applying a Matcher
 *
 * It stores a value which is only evaluated when necessary and an optional additional description for that value.
 *
 * The Expectable object is responsible for creating its own description, based on the value toString method and
 * and an additional description.
 *
 */
case class Expectable[+T] private[specs2](
  actual: () => T,
  checker: Checker = Checker.pass,
  showValue: Option[String => String] = None) { outer =>

  /** the value is only evaluated if necessary */
  lazy val value =
    actual()

  /** definition of the value, possibly evaluating to different results each time it is invoked */
  def valueDefinition = actual()

  /**
   * @return a description of the value provided by the user
   *         a combination of the value show by specs2 and an optional description
   */
  def description: String =
    describe(value)

  /**
   * @return a description of any value with the custom description
   */
  def describe(v: Any): String =
    describeValue(v, showValue)

  /**
   * apply a matcher on the value and return a Result
   */
  def applyMatcher[S >: T](m: =>Matcher[S]): Result =
    val matcher = m
    if matcher == null then throw new IllegalArgumentException(s"You cannot use a null matcher on '$description'")
    checker.check(matcher(this))

  /** evaluate the value and return the same expectable */
  def evaluate = { value; this }

  /** evaluate the value once and return an expectable with the same expression, ready to be evaluated again */
  def evaluateOnce: Expectable[T] =
    copy(actual = () => valueDefinition)

  /**
   * apply a function to the expectable value
   */
  def map[S](f: T => S): Expectable[S] =
    copy(actual = () => f(value))

  /**
   * change the expectable value
   */
  def map[S](other: S): Expectable[S] =
    Expectable[S](() => other, checker, showValue)

  /**
   * apply a function to the description function
   */
  def mapDescription(d: Option[String => String]): Expectable[T] = copy(showValue = d)
  def mapDescription(d: String => String): Expectable[T] = mapDescription(Some(d))
  def mapDescription(d: String): Expectable[T] = mapDescription((_:String) => d)

  /** update the description with another description */
  def updateDescription(d: String => String): Expectable[T] = mapDescription(d(description))
}

trait Checker:
  /** additional checks can be done on the result, such as throwing an exception */
  def check[T](result: Result): Result

object Checker:

  def pass: Checker =
    new Checker:
      def check[T](result: Result): Result =
        result

/**
 * Factory methods for creating Expectables
 */
object Expectable:

  /** @return an Expectable with t as a value, and a constant string for its description */
  private[specs2] def apply[T](t: =>T, d1: =>String): Expectable[T] =
    Expectable(() => t).mapDescription(aliasDisplay(d1))

  /** @return an Expectable with t as a value, and a description function */
  private[specs2] def apply[T](t: =>T, d1: Option[String => String]): Expectable[T] =
    Expectable(() => t).mapDescription(d1)

  /** @return an Expectable with t as a value, and string showing the element t */
  private[specs2] def createWithShowAs[T](t: =>T, show: =>String): Expectable[T] =
    Expectable(() => t).mapDescription(show)

  /** Expectable is a Functor and can use the fmap function to modify its value */
  given ExpectableFunctor: Functor[Expectable] with
    def map[A, B](r: Expectable[A])(f: A => B): Expectable[B] = r.map(f)

  /** @return the description of the matched value, quoted. */
  private[specs2] def describeValue(value: =>Any, showValue: Option[String => String]) =
    showValue match
      case Some(show) =>
        show(value.notNull)

      case _ =>
        value.asInstanceOf[Matchable] match
          case b: Boolean => "the value"
          case _          => value.notNull

  /** @return display a value plus its alias (unless the alias is redundant with the value itself for boolean values). */
  private[specs2] def aliasDisplay(d1: =>String)(s: String): String =
    d1 + (if !s.isEmpty && !Seq("true", "false").contains(s) then " " + q(s) else "")

  /** @return the description of the matched value, unquoted. */
  private[specs2] def dUnquoted[T](value: T, desc: Option[String => String]) = desc match
    case Some(de) => de(unq(value))
    case _ => unq(value)
