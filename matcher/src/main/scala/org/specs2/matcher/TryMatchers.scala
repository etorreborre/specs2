package org.specs2
package matcher

import org.specs2.execute.{AsResult, Failure, Result}
import util.{Try, Success => Succeeded, Failure => Failed}
import scala.reflect.ClassTag
import AnyMatchers.beEqualTo
import MatchersImplicits._
import text.Regexes._
import text.Quote._
import text.NotNullStrings._
import ValueChecks._

/**
 * Matchers for util.Try instances
 */
trait TryMatchers extends TryBaseMatchers with TryBeHaveMatchers
object TryMatchers extends TryMatchers

private[specs2]
trait TryBaseMatchers extends ExceptionMatchers {

  def beSuccessfulTry[T]  = SuccessMatcher[T]()
  def beASuccessfulTry[T] = beSuccessfulTry[T]
  def successfulTry[T]    = beSuccessfulTry[T]
  def aSuccessfulTry[T]   = beSuccessfulTry[T]

  def beFailedTry[T]  = new FailedTryMatcher[T]
  def beAFailedTry[T] = beFailedTry[T]
  def failedTry[T]    = beFailedTry[T]
  def aFailedTry[T]   = beFailedTry[T]
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

case class SuccessMatcher[T]() extends OptionLikeMatcher[Try, T]("a Success", (_:Try[T]).toOption) {
  def withValue(t: ValueCheck[T]) = SuccessCheckedMatcher(t)
}
case class SuccessCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[Try, T]("a Success", (_:Try[T]).toOption, check)

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

  def withThrowable[E <: Throwable : ClassTag](pattern: String) = new Matcher[Try[T]] {
    def apply[S <: Try[T]](value: Expectable[S]) = {
      val expected = implicitly[ClassTag[E]].runtimeClass
      value.value match {
        case Succeeded(_) => result(false, value.description + " is a Failure", value.description + " is not a Failure", value)
        case Failed(e)    => {
          val (assignable, matches) = (expected.isAssignableFrom(e.getClass), e.getMessage.notNull.matchesSafely(pattern))
          result(assignable && matches,
            s"${value.description} is a Failure and ${e.getClass.getName} has a correct type",
            if (!assignable) s"${value.description} is a Failure but '${e.getClass.getName}' is not of type '${expected.getName}'"
            else             s"${value.description} is a Failure but '${e.getMessage.notNull}' doesn't match '$pattern'" ,
            value)
        }
      }
    }
  }
}
