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

  def beSuccessfulTry[T]  = TrySuccessMatcher[T]()
  def beASuccessfulTry[T] = beSuccessfulTry[T]
  def successfulTry[T]    = beSuccessfulTry[T]
  def aSuccessfulTry[T]   = beSuccessfulTry[T]

  def beSuccessfulTry[T] (check: ValueCheck[T]) = TrySuccessCheckedMatcher[T](check)
  def beASuccessfulTry[T](check: ValueCheck[T]) = beSuccessfulTry[T]         (check)
  def successfulTry[T]   (check: ValueCheck[T]) = beSuccessfulTry[T]         (check)
  def aSuccessfulTry[T]  (check: ValueCheck[T]) = beSuccessfulTry[T]         (check)

  def beFailedTry[T]  = new TryFailureMatcher[T]
  def beAFailedTry[T] = beFailedTry[T]
  def failedTry[T]    = beFailedTry[T]
  def aFailedTry[T]   = beFailedTry[T]

  def beFailedTry[T] (check: ValueCheck[Throwable]) = new TryFailureCheckedMatcher[T](check)
  def beAFailedTry[T](check: ValueCheck[Throwable]) = beFailedTry[T]                 (check)
  def failedTry[T]   (check: ValueCheck[Throwable]) = beFailedTry[T]                 (check)
  def aFailedTry[T]  (check: ValueCheck[Throwable]) = beFailedTry[T]                 (check)
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

case class TrySuccessMatcher[T]() extends OptionLikeMatcher[Try, T, T]("a Success", (_:Try[T]).toOption) {
  def withValue(t: ValueCheck[T]) = TrySuccessCheckedMatcher(t)
}
case class TrySuccessCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[Try, T, T]("a Success", (_:Try[T]).toOption, check)

case class TryFailureMatcher[T]() extends OptionLikeMatcher[Try, T, Throwable]("a Failure", (_:Try[T]).failed.toOption) {
  def withValue(t: ValueCheck[Throwable]) = TryFailureCheckedMatcher(t)

  def withThrowable[E <: Throwable : ClassTag] = TryFailureCheckedMatcher[T](ValueChecks.functionIsValueCheck { t: Throwable =>
    createExpectable(t).applyMatcher(AnyMatchers.haveClass[E]).toResult
  })

  def withThrowable[E <: Throwable : ClassTag](pattern: String) = TryFailureCheckedMatcher[T](ValueChecks.functionIsValueCheck { t: Throwable =>
    (createExpectable(t).applyMatcher(AnyMatchers.haveClass[E]) and
     createExpectable(t.getMessage.notNull).applyMatcher(StringMatchers.beMatching(pattern))).toResult
  })
}
case class TryFailureCheckedMatcher[T](check: ValueCheck[Throwable]) extends OptionLikeCheckedMatcher[Try, T, Throwable]("a Failure", (_:Try[T]).failed.toOption, check)
