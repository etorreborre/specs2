package org.specs2
package matcher

import execute._
import control._
import MatchersCreation._
import text.Regexes._

/**
 * Matchers for Operation values
 */
trait OperationMatchers extends ValueChecks {

  def beOk[T]: Matcher[Operation[T]] = (operation: Operation[T]) =>
    AsResult(operation.map(_ => execute.Success()))

  def beOk[T, R : AsResult](f: T => R): Matcher[Operation[T]] = (operation: Operation[T]) =>
    AsResult(operation.map(f))

  def beOk[T](check: ValueCheck[T]): Matcher[Operation[T]] = (operation: Operation[T]) =>
    AsResult(operation.map(check.check))

  def beOkWithValue[T](t: T): Matcher[Operation[T]] =
    beOk(new BeEqualTo(t))

  def beKo[T]: Matcher[Operation[T]] = (operation: Operation[T]) =>
    runOperation(operation).fold(
      e => Success(),
      ok => Failure("a failure was expected")
    )

  def beKo[T](message: String): Matcher[Operation[T]] = (operation: Operation[T]) =>
    runOperation(operation).fold(
      e => e.fold(throwable => if (throwable.getMessage matchesSafely message) Success() else Failure(s"the operation failed with message ${throwable.getMessage}. Expected: $message"),
                  m         => if (m matchesSafely message) Success() else Failure(s"the operation failed with message $m. Expected: $message")),
      ok => Failure(s"a failure with message $message was expected")
    )

}

object OperationMatchers extends OperationMatchers
