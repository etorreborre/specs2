package org.specs2
package matcher

import execute._
import control._
import MatchersCreation._
import text.Regexes._
import fp._, syntax._

/**
 * Matchers for Operation values
 */
trait OperationMatchers extends ValueChecks:

  def beOk[T, R : AsResult](f: T => R): Matcher[Operation[T]] = (operation: Operation[T]) =>
    operation.map(f).runOperation match
      case Left(t) => AsResult.safely[Result](throw t)
      case Right(r) => AsResult(r)

  def beOk[T]: Matcher[Operation[T]] =
    beOk[T, Result]((_:T) => Success())

  def beOk[T](check: ValueCheck[T]): Matcher[Operation[T]] =
    beOk(check.check)

  def beOkWithValue[T](t: T): Matcher[Operation[T]] =
    beOk(new BeEqualTo(t))

  def beKo[T]: Matcher[Operation[T]] = (operation: Operation[T]) =>
    operation.runOperation.fold(
      e => Success(),
      ok => Failure("a failure was expected")
    )

  def beKo[T](message: String): Matcher[Operation[T]] = (operation: Operation[T]) =>
    operation.runOperation.fold(
      throwable => if (throwable.getMessage matchesSafely message) Success() else Failure(s"the operation failed with message ${throwable.getMessage}. Expected: $message"),
      ok => Failure(s"a failure with message $message was expected")
    )


object OperationMatchers extends OperationMatchers
