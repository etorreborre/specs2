package org.specs2
package matcher

import execute.AsResult
import control._
import scalaz.std.anyVal._
import MatchersCreation._

/**
 * Matchers for Action values
 */
trait ControlMatchers extends ValueChecks {

  def beOk[T]: Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(action.map(_ => execute.Success()))

  def beOk[T, R : AsResult](f: T => R): Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(action.map(f))

  def beOk[T](check: ValueCheck[T]): Matcher[Action[T]] = (action: Action[T]) =>
    AsResult(action.map(check.check))

  def beOkWithValue[T](t: T): Matcher[Action[T]] =
    beOk(new BeEqualTo(t))

}

object ControlMatchers extends ControlMatchers
