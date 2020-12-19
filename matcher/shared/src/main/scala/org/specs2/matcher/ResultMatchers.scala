package org.specs2
package matcher

import execute._, Result._
import ResultLogicalCombinators._
import control.Exceptions._
import text.Regexes._
import ValueChecks.{given}
import StringMatchers.{given, _}

/**
 * Matchers for Results
 */
trait ResultMatchers:

  def beSuccessful[T : AsResult]: Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        result(ResultExecution.execute(AsResult[T](value.value)).isSuccess,
               value.description + " is not a success")

  def beFailing[T : AsResult]: Matcher[T] =
    beFailing(ValueCheck.alwaysOk[String])

  def beFailing[T : AsResult](message: String): Matcher[T] =
    beFailing(beMatching(message.regexPart))

  def beFailing[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val r = ResultExecution.execute(AsResult[T](value.value))
        def description = tryOrElse(value.description)(r.toString)
        result(r.isFailure, description + " is not a failure") and
        check.check(r.message)

  def beError[T : AsResult]: Matcher[T] =
    beError(ValueCheck.alwaysOk[String])

  def beError[T : AsResult](message: String): Matcher[T] =
    beError(beMatching(message.regexPart))

  def beError[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val r = ResultExecution.execute(AsResult[T](value.value))
        def description = tryOrElse(value.description)(r.toString)
        result(r.isError, description + " is not an error") and
        check.check(r.message)

  def beSkipped[T : AsResult]: Matcher[T] =
    beSkipped(ValueCheck.alwaysOk[String])

  def beSkipped[T : AsResult](message: String): Matcher[T] =
    beSkipped(beMatching(message.regexPart))

  def beSkipped[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val r = ResultExecution.execute(AsResult[T](value.value))
        def description = tryOrElse(value.description)(r.toString)
        result(r.isSkipped, description + " is not skipped") and
        check.check(r.message)

  def bePending[T : AsResult]: Matcher[T] =
    bePending(ValueCheck.alwaysOk[String])

  def bePending[T : AsResult](message: String): Matcher[T] =
    bePending(beMatching(message.regexPart))

  def bePending[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val r = ResultExecution.execute(AsResult[T](value.value))
        def description = tryOrElse(value.description)(r.toString)
        result(r.isPending, description + " is pending") and
        check.check(r.message)

object ResultMatchers extends ResultMatchers
