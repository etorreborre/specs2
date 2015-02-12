package org.specs2
package matcher

import execute._
import ValueChecks._

/**
 * Matchers for Options
 */
trait OptionMatchers extends OptionBaseMatchers with OptionBeHaveMatchers with ValueChecks

object OptionMatchers extends OptionMatchers

private[specs2]
trait OptionBaseMatchers {

  def beSome[T](check: ValueCheck[T]): SomeCheckedMatcher[T] = SomeCheckedMatcher(check)
  def some[T](t: T): SomeCheckedMatcher[T] = beSome(t)
  def some[T](check: ValueCheck[T]): SomeCheckedMatcher[T] = beSome(check)

  def beSome[T]: SomeMatcher[T] = new SomeMatcher[T]
  def some[T]: SomeMatcher[T] = beSome[T]

  def beNone: Matcher[Option[Any]] = new Matcher[Option[Any]] {
    def apply[S <: Option[Any]](value: Expectable[S]) = {
      result(value.value == None,
             value.description + " is None",
             value.description + " is not None",
             value)
    }
  }

  def none: Matcher[Option[Any]] = beNone

  def beAsNoneAs[T](other: =>Option[T]): Matcher[Option[T]] = new Matcher[Option[T]] {
    def apply[S <: Option[T]](a: Expectable[S]) = {
      val b = other
      result(a.value == None && b == None || a.value != None && b != None, 
             a.description + " is None as well",
             if (a.value == None) b + " is not None" else a.description + " is not None",
             a)
    }
  }

  def asNoneAs[T](other: =>Option[T]): Matcher[Option[T]] =
    beAsNoneAs(other)
}

private[specs2]
trait OptionBeHaveMatchers extends BeHaveMatchers { outer: OptionBaseMatchers =>
  implicit def toOptionResultMatcher[T](result: MatchResult[Option[T]]) = new OptionResultMatcher(result)
  class OptionResultMatcher[T](result: MatchResult[Option[T]]) {
    def beSome = result(outer.beSome)
    def beSome(check: ValueCheck[T]) = result(outer.beSome(check))
    def beNone = result(outer.beNone)
    def some = result(outer.beSome)
    def some(check: ValueCheck[T]) = result(outer.beSome(check))
    def none = result(outer.beNone)
    def asNoneAs(other: =>Option[T]) = result(beAsNoneAs(other))
  }
}

case class SomeMatcher[T]() extends OptionLikeMatcher[Option, T, T]("Some", identity)
case class SomeCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[Option, T, T]("Some", identity, check)

class OptionLikeMatcher[F[_], T, U](typeName: String, toOption: F[T] => Option[U]) extends Matcher[F[T]] {
  def apply[S <: F[T]](value: Expectable[S]) =
    result(toOption(value.value).isDefined, s"${value.description} is $typeName", s"${value.description} is not $typeName", value)

  def which[R : AsResult](f: U => R) = new OptionLikeCheckedMatcher(typeName, toOption, f)
  def like[R : AsResult](f: PartialFunction[U, R]) = new OptionLikeCheckedMatcher(typeName, toOption, f)
}

class OptionLikeCheckedMatcher[F[_], T, U](typeName: String, toOption: F[T] => Option[U], check: ValueCheck[U]) extends Matcher[F[T]] {
  def apply[S <: F[T]](value: Expectable[S]) = {
    toOption(value.value) match {
      case Some(v) =>
        val r = check.check(v)
        val (okMessage, koMessage) = (s"${value.description} is $typeName and ${r.message}", s"${value.description} is $typeName but ${r.message}")
        r match {
          case f @ Failure(_,_,_,details) => result(r.isSuccess, okMessage, koMessage, value, details)
          case _ =>                          result(r.isSuccess, okMessage, koMessage, value)
        }

      case None => result(false, s"${value.description} is $typeName", s"${value.description} is not $typeName", value)
    }
  }
}
