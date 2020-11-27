package org.specs2
package matcher

import execute._
import MatchResultLogicalCombinators._
import control.Exceptions._
import text.Regexes._
import ValueChecks.{given}

/**
 * Matchers for Results
 */
trait ResultMatchers:

  def beSuccessful[T : AsResult]: Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        result(ResultExecution.execute(AsResult[T](value.value)).isSuccess,
               value.description + " is a success",
               value.description + " is not a success",
               value)

  def beFailing[T : AsResult]: Matcher[T] =
    beFailing(ValueCheck.alwaysOk[String])

  def beFailing[T : AsResult](message: String): Matcher[T] =
    beFailing(new BeMatching(message.regexPart))

  def beFailing[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val r = ResultExecution.execute(AsResult[T](value.value))
        def description = tryOrElse(value.description)(r.toString)
        result(r.isFailure,
               description + " is a failure",
               description + " is not a failure",
               value) and
        result(check.check(r.message), value)

  def beError[T : AsResult]: Matcher[T] =
    beError(ValueCheck.alwaysOk[String])

  def beError[T : AsResult](message: String): Matcher[T] =
    beError(new BeMatching(message.regexPart))

  def beError[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val r = ResultExecution.execute(AsResult[T](value.value))
        def description = tryOrElse(value.description)(r.toString)
        result(r.isError,
               description + " is an error",
               description + " is not an error",
               value) and
        result(check.check(r.message), value)

  def beSkipped[T : AsResult]: Matcher[T] =
    beSkipped(ValueCheck.alwaysOk[String])

  def beSkipped[T : AsResult](message: String): Matcher[T] =
    beSkipped(new BeMatching(message.regexPart))

  def beSkipped[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val r = ResultExecution.execute(AsResult[T](value.value))
        def description = tryOrElse(value.description)(r.toString)
        result(r.isSkipped,
               description + " is skipped",
               description + " is not skipped",
               value) and
        result(check.check(r.message), value)

  def bePending[T : AsResult]: Matcher[T] =
    bePending(ValueCheck.alwaysOk[String])

  def bePending[T : AsResult](message: String): Matcher[T] =
    bePending(new BeMatching(message.regexPart))

  def bePending[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val r = ResultExecution.execute(AsResult[T](value.value))
        def description = tryOrElse(value.description)(r.toString)
        result(r.isPending,
          description + " is pending",
          description + " is not pending",
          value) and
        result(check.check(r.message), value)

object ResultMatchers extends ResultMatchers
