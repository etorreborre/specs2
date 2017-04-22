package org.specs2
package matcher

import execute._
import control._
import MatchersCreation._
import org.specs2.concurrent.ExecutionEnv
import text.Regexes._

/**
 * Matchers for Action values
 */
trait ActionMatchers extends ValueChecks {

  private lazy val actionMatchersExecutionEnv = ExecutionEnv.fromGlobalExecutionContext

  def beOk[T]: Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(runAction(action)(actionMatchersExecutionEnv).fold(t => t.fold(Error(_), Error(_)), _ => Success()))

  def beOk[T, R : AsResult](f: T => R): Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(runAction(action)(actionMatchersExecutionEnv).fold(t => t.fold(Error(_), Error(_)), t => AsResult(f(t))))

  def beOk[T](check: ValueCheck[T]): Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(runAction(action)(actionMatchersExecutionEnv).fold(t => t.fold(Error(_), Error(_)), check.check))

  def beOkWithValue[T](t: T): Matcher[Action[T]] =
    beOk(new BeEqualTo(t))

  def beKo[T]: Matcher[Action[T]] = (action: Action[T]) =>
    runAction(action)(actionMatchersExecutionEnv).fold(
      e => Success(),
      ok => Failure("a failure was expected")
    )

  def beKo[T](message: String): Matcher[Action[T]] = (action: Action[T]) =>
    runAction(action)(actionMatchersExecutionEnv).fold(
      e => e.fold(throwable => if (throwable.getMessage matchesSafely message) Success() else Failure(s"the action failed with message ${throwable.getMessage}. Expected: $message"),
                  m         => if (m matchesSafely message) Success() else Failure(s"the action failed with message $m. Expected: $message")),
      ok => Failure(s"a failure with message $message was expected")
    )

}

object ActionMatchers extends ActionMatchers
