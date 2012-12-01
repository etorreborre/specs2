package org.specs2
package matcher
import text.Quote._
import execute.{ Failure, Result }

import scalaz.{ Failure => ZFailure, Success => ZSuccess, Validation }

trait ValidationMatchers extends ValidationBaseMatchers with ValidationBeHaveMatchers
object ValidationMatchers extends ValidationMatchers

private[specs2] trait ValidationBaseMatchers {

  def beZSuccess[T](t: => T) = new Matcher[Validation[_, T]] {
    def apply[S <: Validation[_, T]](value: Expectable[S]) = {
      val expected = t
      result(
        value.value == ZSuccess(t),
        value.description + " is Success with value" + q(expected),
        value.description + " is not Success with value" + q(expected),
        value
      )
    }
  }

  def beZSuccess[T] = new ZSuccessMatcher[T]
  class ZSuccessMatcher[T] extends Matcher[Validation[_, T]] {
    def apply[S <: Validation[_, T]](value: Expectable[S]) = {
      result(
        value.value.isSuccess,
        value.description + " is Success",
        value.description + " is not Success",
        value
      )
    }

    def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

    private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) =
      new Matcher[Validation[_, T]] {

      def apply[S <: Validation[_, T]](value: Expectable[S]) = {
        val res: Result = value.value match {
          case ZSuccess(t) if f.isDefinedAt(t)  => f(t).toResult
          case ZSuccess(t) if !f.isDefinedAt(t) => Failure("function undefined")
          case other                            => Failure("no match")
        }
        result(
          res.isSuccess,
          value.description + " is Success[T] and " + res.message,
          value.description + " is Success[T] but " + res.message,
          value
        )
      }
    }
  }

  def zsuccess[T](t: => T) = beZSuccess(t)
  def zsuccess[T] = beZSuccess

  def beZFailure[T](t: => T) = new Matcher[Validation[T, _]] {
    def apply[S <: Validation[T, _]](value: Expectable[S]) = {
      val expected = t
      result(
        value.value == ZFailure(t),
        value.description + " is Failure with value" + q(expected),
        value.description + " is not Failure with value" + q(expected),
        value
      )
    }
  }

  def beZFailure[T] = new ZFailureMatcher[T]
  class ZFailureMatcher[T] extends Matcher[Validation[T, _]] {
    def apply[S <: Validation[T, _]](value: Expectable[S]) = {
      result(
        value.value.isFailure,
        value.description + " is Failure",
        value.description + " is not Failure",
        value
      )
    }

    def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

    private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) =
      new Matcher[Validation[T, _]] {

      def apply[S <: Validation[T, _]](value: Expectable[S]) = {
        val res: Result = value.value match {
          case ZFailure(t) if f.isDefinedAt(t)  => f(t).toResult
          case ZFailure(t) if !f.isDefinedAt(t) => Failure("function undefined")
          case other                            => Failure("no match")
        }
        result(
          res.isSuccess, //specs2's Result#isSuccess method!
          value.description + " is Failure[T] and " + res.message,
          value.description + " is Failure[T] but " + res.message,
          value
        )
      }
    }
  }

  def zfailure[T](t: => T) = beZFailure(t)
  def zfailure[T] = beZFailure
}


private[specs2] trait ValidationBeHaveMatchers { outer: ValidationBaseMatchers =>

  implicit def toValidationResultMatcher[F, S](result: MatchResult[Validation[F, S]]) =
    new ValidationResultMatcher(result)

  class ValidationResultMatcher[F, S](result: MatchResult[Validation[F, S]]) {
    def zfailure(f: => F) = result(outer beZFailure f)
    def zsuccess(s: => S) = result(outer beZSuccess s)
    def beZFailure(f: => F) = result(outer beZFailure f)
    def beZSuccess(s: => S) = result(outer beZSuccess s)

    def zfailure = result(outer.beZFailure)
    def zsuccess = result(outer.beZSuccess)
    def beZFailure = result(outer.beZFailure)
    def beZSuccess = result(outer.beZSuccess)
  }
}
