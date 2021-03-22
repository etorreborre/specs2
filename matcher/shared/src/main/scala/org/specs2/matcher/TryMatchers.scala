package org.specs2
package matcher

import language.adhocExtensions
import scala.reflect.ClassTag
import util.*
import execute.ResultImplicits.*
import org.specs2.matcher.describe.Diffable
import text.NotNullStrings.*
import ValueChecks.{given}
import StringMatchers.{given, *}

/**
 * Matchers for util.Try instances
 */
trait TryMatchers:

  def beSuccessfulTry[T]: TrySuccessMatcher[T] =
    TrySuccessMatcher[T]()

  def beASuccessfulTry[T]: TrySuccessMatcher[T] =
    beSuccessfulTry[T]

  def aSuccessfulTry[T]: TrySuccessMatcher[T] =
    beSuccessfulTry[T]

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
    Expectations.createExpectable(t).applyMatcher(AnyMatchers.beAnInstanceOf[E])
  })

  def withThrowable[E <: Throwable : ClassTag](pattern: String) = TryFailureCheckedMatcher[T]({ (t: Throwable) =>
    (Expectations.createExpectable(t).applyMatcher(AnyMatchers.beAnInstanceOf[E]) and
     Expectations.createExpectable(t.getMessage.notNull).applyMatcher(StringMatchers.beMatching(pattern)))
  })
case class TryFailureCheckedMatcher[T](check: ValueCheck[Throwable]) extends OptionLikeCheckedMatcher[Try, T, Throwable]("a Failure", (_:Try[T]).failed.toOption, check)
