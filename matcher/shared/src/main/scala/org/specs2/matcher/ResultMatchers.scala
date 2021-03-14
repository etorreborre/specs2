package org.specs2
package matcher

import execute.*, Result.*
import ResultLogicalCombinators.*
import control.Exceptions.*
import text.Regexes.*
import ValueChecks.{given}
import StringMatchers.{given, *}

/**
 * Matchers for Results
 */
trait ResultMatchers:

  def beSuccessful[T : AsResult]: Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val (r, description) = ResultExecution.executeEither(AsResult[T](value.value)) match
          case Left(r) => (r, "the value is not a success: ")
          case Right(r) => (r, value.description + " is not a success")
        result(r.isSuccess, description+ r.message)

  def beFailing[T : AsResult]: Matcher[T] =
    beFailing(ValueCheck.alwaysOk[String])

  def beFailing[T : AsResult](message: String): Matcher[T] =
    beFailing(beMatching(message.regexPart))

  def beFailing[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]): Result =
        val (r, description) = ResultExecution.executeEither(AsResult[T](value.value)) match
          case Left(r) => (r, "the value is not a failure: ")
          case Right(r) => (r, value.description + " is not a failure")
        result(r.isFailure, description+ r.message) `and` check.check(r.message)

  def beError[T : AsResult]: Matcher[T] =
    beError(ValueCheck.alwaysOk[String])

  def beError[T : AsResult](message: String): Matcher[T] =
    beError(beMatching(message.regexPart))

  def beError[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val (r, description) = ResultExecution.executeEither(AsResult[T](value.value)) match
          case Left(r) => (r, "the value is not an error: ")
          case Right(r) => (r, value.description + " is not an error")
        result(r.isError, description+ r.message) `and` check.check(r.message)

  def beSkipped[T : AsResult]: Matcher[T] =
    beSkipped(ValueCheck.alwaysOk[String])

  def beSkipped[T : AsResult](message: String): Matcher[T] =
    beSkipped(beMatching(message.regexPart))

  def beSkipped[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
        val (r, description) = ResultExecution.executeEither(AsResult[T](value.value)) match
          case Left(r) => (r, "the value is not skipped: ")
          case Right(r) => (r, value.description + " is not skipped")
        result(r.isSkipped, description+ r.message) `and` check.check(r.message)

  def bePending[T : AsResult]: Matcher[T] =
    bePending(ValueCheck.alwaysOk[String])

  def bePending[T : AsResult](message: String): Matcher[T] =
    bePending(beMatching(message.regexPart))

  def bePending[T : AsResult](check: ValueCheck[String]): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](value: Expectable[S]) =
       println(value.value)
        val (r, description) = ResultExecution.executeEither(AsResult[T](value.value)) match
          case Left(r) => (r, "the value is not pending: ")
          case Right(r) => (r, value.description + " is not pending")
        result(r.isPending, description+ r.message) `and` check.check(r.message)

object ResultMatchers extends ResultMatchers
