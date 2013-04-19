package org.specs2
package matcher

import execute.{Failure, Result}
import util.{Try, Success => Succeeded, Failure => Failed}
import scala.reflect.ClassTag
import AnyMatchers.beEqualTo
import MatchersImplicits._
import text.Regexes._
import text.Quote._
/**
 * Matchers for util.Try instances
 */
trait TryMatchers extends TryBaseMatchers with TryBeHaveMatchers
object TryMatchers extends TryMatchers

private[specs2]
trait TryBaseMatchers extends ExceptionMatchers {

  def beSuccessfulTry[T]  = SuccessTryMatcher[T]()
  def beASuccessfulTry[T] = beSuccessfulTry[T]
  def successfulTry[T]    = beSuccessfulTry[T]
  def aSuccessfulTry[T]   = beSuccessfulTry[T]

  case class SuccessTryMatcher[T]() extends  Matcher[Try[T]] { outer =>
    def apply[S <: Try[T]](value: Expectable[S]) =
      result(value.value.isSuccess,
             s"${value.description} is a Success",
             s"${value.description} is not a Success",
             value)

    def withValue(t: =>T) = new Matcher[Try[T]] {
      def apply[S <: Try[T]](value: Expectable[S]) =
        value.value match {
          case Succeeded(v) => {
            val expected = t
            val koEqual = beEqualTo(expected).apply(Expectable(v)).message
            result(v == expected,
                   s"${value.description} is a Success with value ${q(expected)}",
                   s"${value.description} is a Success but the value is incorrect:\n  $koEqual", value)
          }
          case Failed(_) => result(false, s"${value.description} is a Success", s"${value.description} is not a Success", value)
        }
    }

    def which(f: T => Boolean) = this ^^ { (t: Try[T]) => t filter f }
    def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

    private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) = new Matcher[Try[T]] {
      def apply[S <: Try[T]](value: Expectable[S]) = {
        val res: Result = value.value match {
          case Succeeded(t) if f.isDefinedAt(t)  => f(t).toResult
          case Succeeded(t) if !f.isDefinedAt(t) => Failure("the function is undefined on this value")
          case Failed(_)                         => Failure("no match")
        }
        result(res.isSuccess,
               s"${value.description} is a Success and ${res.message}",
               s"${value.description} is a Success but ${res.message}",
               value)
      }
    }
  }

  def beFailedTry[T]  = new FailedTryMatcher[T]
  def beAFailedTry[T] = beFailedTry[T]
  def failedTry[T]    = beFailedTry[T]
  def aFailedTry[T]   = beFailedTry[T]

  case class FailedTryMatcher[T]() extends Matcher[Try[T]] { outer =>
    def apply[S <: Try[T]](value: Expectable[S]) = {
      result(value.value.isFailure,
             s"${value.description} is a Failure",
             s"${value.description} is not a Failure",
             value)
    }
    def withThrowable[E <: Throwable : ClassTag] = new Matcher[Try[T]] {
      def apply[S <: Try[T]](value: Expectable[S]) = {
        val expected = implicitly[ClassTag[E]].runtimeClass
        value.value match {
          case Succeeded(_) => result(false, value.description + " is a Failure", value.description + " is not a Failure", value)
          case Failed(e)    => result(expected.isAssignableFrom(e.getClass),
                                      s"${value.description} is a Failure and '${e.getClass.getName}' has a correct type",
                                      s"${value.description} is a Failure but '${e.getClass.getName}' is not of type '${expected.getName}'",
                                      value)
        }
      }
    }
    def withThrowable[E <: Throwable : ClassTag](message: String) = new Matcher[Try[T]] {
      def apply[S <: Try[T]](value: Expectable[S]) = {
        val expected = implicitly[ClassTag[E]].runtimeClass
        value.value match {
          case Succeeded(_) => result(false, value.description + " is a Failure", value.description + " is not a Failure", value)
          case Failed(e)    => {
            val (assignable, matches) = (expected.isAssignableFrom(e.getClass), e.getMessage.matchesSafely(message))
            result(assignable && matches,
                   s"${value.description} is a Failure and ${e.getClass.getName} has a correct type",
                   if (!assignable) (s"${value.description} is a Failure but '${e.getClass.getName}' is not of type '${expected.getName}'")
                   else             (s"${value.description} is a Failure but '${e.getMessage}' doesn't match '$message'") ,
                   value)
          }
        }
      }
    }
  }

}

private[specs2]
trait TryBeHaveMatchers { outer: TryBaseMatchers =>

  implicit def toTryResultMatcher[T](result: MatchResult[Try[T]]) = new TryResultMatcher(result)
  class TryResultMatcher[T](result: MatchResult[Try[T]]) {
    def beSuccessfulTry = result(outer.beSuccessfulTry)
    def beASuccessfulTry = result(outer.beSuccessfulTry)
    def successfulTry = result(outer.beSuccessfulTry)
    def aSuccessfulTry = result(outer.beSuccessfulTry)
    def beFailedTry = result(outer.beFailedTry)
    def beAFailedTry = result(outer.beFailedTry)
    def aFailedTry = result(outer.beFailedTry)
    def failedTry = result(outer.beFailedTry)
  }
}
