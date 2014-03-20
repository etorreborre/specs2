package org.specs2
package matcher

import text.Quote._
import execute._
import text.NotNullStrings._
import ValueChecks._

/**
 * Matchers for Options
 */
trait OptionMatchers extends OptionBaseMatchers with OptionBeHaveMatchers
object OptionMatchers extends OptionMatchers

private[specs2]
trait OptionBaseMatchers {
  
  def beSome[T](check: ValueCheck[T]) = SomeCheckedMatcher(check)

  def some[T](check: ValueCheck[T]) = beSome(check)

  def beSome[T] = new SomeMatcher[T]
  def some[T] = beSome[T]

  def beNone = new Matcher[Option[Any]] {
    def apply[S <: Option[Any]](value: Expectable[S]) = {
      result(value.value == None,
             value.description + " is None",
             value.description + " is not None",
             value)
    }
  }

  def none = beNone
  def beAsNoneAs[T](other: =>Option[T]) = new Matcher[Option[T]] {
    def apply[S <: Option[T]](a: Expectable[S]) = {
      val b = other
      result(a.value == None && b == None || a.value != None && b != None, 
             a.description + " is None as well",
             if (a.value == None) b + " is not None" else a.description + " is not None",
             a)
    }
  }

  def asNoneAs[T](other: =>Option[T]) = beAsNoneAs(other)
}

private[specs2]
trait OptionBeHaveMatchers { outer: OptionBaseMatchers =>
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
      case Some(v) => {
        val r = check.check(v)
        result(r.isSuccess, s"${value.description} is $typeName and ${r.message}", s"${value.description} is $typeName but ${r.message}", value)
      }
      case None    => result(false, s"${value.description} is $typeName", s"${value.description} is not $typeName", value)
    }
  }
}
