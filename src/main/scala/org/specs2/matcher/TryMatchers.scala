package org.specs2
package matcher

import execute.{Failure, Result}
import util.{Try, Success => Succeeded, Failure => Failed}
import scala.reflect.ClassTag
import AnyMatchers.beEqualTo
import MatchersImplicits._

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
             value.description + " is a Success",
             value.description + " is not a Success",
             value)

    def withValue(t: =>T) = new Matcher[Try[T]] {
      def apply[S <: Try[T]](value: Expectable[S]) =
        outer(value) and result(beEqualTo(t)(value.map((_:Try[T]).get)), value)
    }

    def which(f: T => Boolean) = this ^^ { (t: Try[T]) => t filter f }
    def like(f: PartialFunction[T, MatchResult[_]]) = this and partialMatcher(f)

    private def partialMatcher(f: PartialFunction[T, MatchResult[_]]) = new Matcher[Try[T]] {
      def apply[S <: Try[T]](value: Expectable[S]) = {
        val res: Result = value.value match {
          case Succeeded(t) if f.isDefinedAt(t)  => f(t).toResult
          case Succeeded(t) if !f.isDefinedAt(t) => Failure("function undefined")
          case Failed(_)                         => Failure("no match")
        }
        result(res.isSuccess,
               value.description+" is a Success and "+res.message,
               value.description+" is a Success but "+res.message,
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
        value.description + " is a Failure",
        value.description + " is not a Failure",
        value)
    }
    def withThrowable[E <: Throwable : ClassTag] = new Matcher[Try[T]] {
      def apply[S <: Try[T]](value: Expectable[S]) =
        outer(value) and haveThrowable[T, E].apply(value)
    }
    def withThrowable[E <: Throwable : ClassTag](message: String) = new Matcher[Try[T]] {
      def apply[S <: Try[T]](value: Expectable[S]) =
        outer(value) and haveThrowable[T, E](message).apply(value)
    }
  }

  private def haveThrowable[T, E <: Throwable : ClassTag] = new Matcher[Try[T]] {
    def apply[S <: Try[T]](value: Expectable[S]) =
      value.value match {
        case Succeeded(_) => result(false, "ok", "no throwable", value)
        case Failed(e)    => result(throwA[E].apply(thrownExpectable[T, S](value)), value)
      }
  }
  private def haveThrowable[T, E <: Throwable : ClassTag](message: String) = new Matcher[Try[T]] {
    def apply[S <: Try[T]](value: Expectable[S]) = {
      value.value match {
        case Succeeded(_) => result(false, "ok", "no throwable", value)
        case Failed(e)    => result(throwA[E](message).apply(thrownExpectable[T, S](value)), value)
      }
    }
  }
  private def thrownExpectable[T, S <: Try[T]](value: Expectable[S]) = value.map { (v: Try[T]) =>
    v match {
      case Failed(t) => throw t; t
      case _ => v
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
