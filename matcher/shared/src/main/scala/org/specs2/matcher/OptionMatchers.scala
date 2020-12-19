package org.specs2
package matcher

import execute._
import ValueChecks.{given, _}
import org.specs2.control._, ImplicitParameters._
import org.specs2.matcher.describe.Diffable
import Result._

/**
 * Matchers for Options
 */
trait OptionMatchers extends ValueChecks:

  def beSome[T](check: ValueCheck[T]): SomeCheckedMatcher[T] =
    SomeCheckedMatcher(check)

  def some[T : Diffable](t: T): SomeCheckedMatcher[T] =
    beSome(ValueChecks.valueIsTypedValueCheck(t))

  def some[T](check: ValueCheck[T]): SomeCheckedMatcher[T] =
    beSome(check)

  def beSome[T](using p: ImplicitParam = ImplicitParameters.implicitParameter): SomeMatcher[T] =
    use(p)(new SomeMatcher[T])

  def some[T]: SomeMatcher[T] = beSome[T]

  def beNone: Matcher[Option[Any]] =
    new Matcher[Option[Any]]:
      def apply[S <: Option[Any]](value: Expectable[S]) =
        result(value.value == None, value.description + " is not None")

  def none: Matcher[Option[Any]] =
    beNone

  def beAsNoneAs[T](other: =>Option[T]): Matcher[Option[T]] =
    new Matcher[Option[T]]:
      def apply[S <: Option[T]](a: Expectable[S]) =
        val b = other
        result(a.value == None && b == None || a.value != None && b != None,
               if a.value == None then b.toString + " is not None" else a.description + " is not None")

  def asNoneAs[T](other: =>Option[T]): Matcher[Option[T]] =
    beAsNoneAs(other)

object OptionMatchers extends OptionMatchers

case class SomeMatcher[T]() extends OptionLikeMatcher[Option, T, T]("Some", identity)
case class SomeCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[Option, T, T]("Some", identity, check)

class OptionLikeMatcher[F[_], T, U](typeName: String, toOption: F[T] => Option[U]) extends Matcher[F[T]]:
  def apply[S <: F[T]](value: Expectable[S]) =
    result(toOption(value.value).isDefined, s"${value.description} is not $typeName")

  def which[R : AsResult](f: U => R): OptionLikeCheckedMatcher[F, T, U] =
    new OptionLikeCheckedMatcher(typeName, toOption, f)

  def like[R : AsResult](f: PartialFunction[U, R]): OptionLikeCheckedMatcher[F, T, U] =
    new OptionLikeCheckedMatcher(typeName, toOption, f)

class OptionLikeCheckedMatcher[F[_], T, U](typeName: String, toOption: F[T] => Option[U], check: ValueCheck[U]) extends Matcher[F[T]]:
  def apply[S <: F[T]](value: Expectable[S]) =
    toOption(value.value) match
      case Some(v) =>
        val r = check.check(v)
        val koMessage = s"${value.description} is $typeName but ${r.message}"
        r match
          case f @ Failure(_,_,_,details) => result(r.isSuccess, koMessage, details)
          case _ =>                          result(r.isSuccess, koMessage)

      case None =>
        result(false, s"${value.description} is not $typeName")
