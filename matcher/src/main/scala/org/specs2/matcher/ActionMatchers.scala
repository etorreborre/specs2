package org.specs2
package matcher

import execute._
import control._
import MatchersCreation._
import text.Regexes._

/**
 * Matchers for Action values
 */
trait ActionMatchers extends ValueChecks {

  def beOk[T]: Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(action.map(_ => execute.Success()))

  def beOk[T, R : AsResult](f: T => R): Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(action.map(f))

  def beOk[T](check: ValueCheck[T]): Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(action.map(check.check))

  def beOkWithValue[T](t: T): Matcher[Action[T]] =
    beOk(new BeEqualTo(t))

  def beKo[T]: Matcher[Action[T]] = (action: Action[T]) =>
    action.execute(noLogging).unsafePerformIO.foldAll(
      ok     => Failure("a failure was expected"),
      m      => Success(),
      t      => Success(),
      (m, t) => Success()
    )

  def beKo[T](message: String): Matcher[Action[T]] = (action: Action[T]) =>
    action.execute(noLogging).unsafePerformIO.foldAll(
      ok        => Failure(s"a failure with message $message was expected"),
      m         => if (m matchesSafely message) Success() else Failure(s"the action failed with message $m. Expected: $message"),
      throwable => if (throwable.getMessage matchesSafely message) Success() else Failure(s"the action failed with message ${throwable.getMessage}. Expected: $message"),
      (m, t)    => if (m matchesSafely message) Success() else Failure(s"the action failed with message $m. Expected: $message")
    )

}

object ActionMatchers extends ActionMatchers
