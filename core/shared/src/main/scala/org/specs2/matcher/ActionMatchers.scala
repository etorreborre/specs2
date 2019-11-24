package org.specs2
package matcher

import control._
import execute._
import matcher.MatchersImplicits._
import text.Regexes._
import scala.concurrent.ExecutionContext.global

/**
 * Matchers for Action values
 */
trait ActionMatchers extends ValueChecks {

  def beOk[T]: Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(action.runAction(global).fold(t => Error(t), _ => Success()))

  def beOk[T, R : AsResult](f: T => R): Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(action.runAction(global).fold(t => Error(t), t => AsResult(f(t))))

  def beOk[T](check: ValueCheck[T]): Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(action.runAction(global).fold(t => Error(t), check.check))

  def beOkWithValue[T](t: T): Matcher[Action[T]] =
    beOk(new BeEqualTo(t))

  def beKo[T]: Matcher[Action[T]] = (action: Action[T]) =>
    action.runAction(global).fold(
      e => Success(),
      ok => Failure("a failure was expected")
    )

  def beKo[T](message: String): Matcher[Action[T]] = (action: Action[T]) =>
    action.runAction(global).fold(
      throwable => if (throwable.getMessage matchesSafely message) Success() else Failure(s"the action failed with message ${throwable.getMessage}. Expected: $message"),
      ok => Failure(s"a failure with message $message was expected")
    )

}

object ActionMatchers extends ActionMatchers
