package org.specs2
package matcher

import util._
import scala.reflect.ClassTag
import MatchersImplicits._
import org.specs2.matcher.describe.Diffable
import text.NotNullStrings._
import ValueChecks.{given}

/**
 * Matchers for util.Try instances
 */
trait TryMatchers:

  def beSuccessfulTry[T]  = TrySuccessMatcher[T]()
  def beASuccessfulTry[T] = beSuccessfulTry[T]
  def successfulTry[T]    = beSuccessfulTry[T]
  def aSuccessfulTry[T]   = beSuccessfulTry[T]

  def beSuccessfulTry[T] (check: ValueCheck[T]) = TrySuccessCheckedMatcher[T](check)
  def beASuccessfulTry[T](check: ValueCheck[T]) = beSuccessfulTry[T]         (check)
  def successfulTry[T]   (check: ValueCheck[T]) = beSuccessfulTry[T]         (check)
  def aSuccessfulTry[T]  (check: ValueCheck[T]) = beSuccessfulTry[T]         (check)

  def successfulTry[T : Diffable](t: T) = beSuccessfulTry[T](t)
  def aSuccessfulTry[T : Diffable](t: T) = beSuccessfulTry[T](t)

  def beFailedTry[T]  = new TryFailureMatcher[T]
  def beAFailedTry[T] = beFailedTry[T]
  def failedTry[T]    = beFailedTry[T]
  def aFailedTry[T]   = beFailedTry[T]

  def beFailedTry[T] (check: ValueCheck[Throwable]) = new TryFailureCheckedMatcher[T](check)
  def beAFailedTry[T](check: ValueCheck[Throwable]) = beFailedTry[T]                 (check)
  def failedTry[T]   (check: ValueCheck[Throwable]) = beFailedTry[T]                 (check)
  def aFailedTry[T]  (check: ValueCheck[Throwable]) = beFailedTry[T]                 (check)

  def failedTry[T]   (t: Throwable) = beFailedTry[T](t)
  def aFailedTry[T]  (t: Throwable) = beFailedTry[T](t)

object TryeMatchers extends TryMatchers

case class TrySuccessMatcher[T]() extends OptionLikeMatcher[Try, T, T]("a Success", (_:Try[T]).toOption):
  def withValue(t: ValueCheck[T]) = TrySuccessCheckedMatcher(t)
case class TrySuccessCheckedMatcher[T](check: ValueCheck[T]) extends OptionLikeCheckedMatcher[Try, T, T]("a Success", (_:Try[T]).toOption, check)

case class TryFailureMatcher[T]() extends OptionLikeMatcher[Try, T, Throwable]("a Failure", (_:Try[T]).failed.toOption):
  def withValue(t: ValueCheck[Throwable]) = TryFailureCheckedMatcher(t)

  def withThrowable[E <: Throwable : ClassTag] = TryFailureCheckedMatcher[T]({ (t: Throwable) =>
    Expectations.createExpectable(t).applyMatcher(AnyMatchers.beAnInstanceOf[E]).toResult
  })

  def withThrowable[E <: Throwable : ClassTag](pattern: String) = TryFailureCheckedMatcher[T]({ (t: Throwable) =>
    (Expectations.createExpectable(t).applyMatcher(AnyMatchers.beAnInstanceOf[E]) and
     Expectations.createExpectable(t.getMessage.notNull).applyMatcher(StringMatchers.beMatching(pattern))).toResult
  })
case class TryFailureCheckedMatcher[T](check: ValueCheck[Throwable]) extends OptionLikeCheckedMatcher[Try, T, Throwable]("a Failure", (_:Try[T]).failed.toOption, check)
