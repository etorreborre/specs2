package org.specs2
package matcher

import execute._
import text.Quote._

/**
 * Matchers for Results
 */
trait ResultMatchers extends ResultBaseMatchers with ResultBeHaveMatchers
object ResultMatchers extends ResultMatchers

private[specs2]
trait ResultBaseMatchers {

  def beSuccessful[T](implicit toResult: T => Result) = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      result(toResult(value.value).isSuccess,
             value.description + " is a success",
             value.description + " is not a success",
             value)
    }
  }

  def beFailing[T <% Result]: Matcher[T] = beFailing(None)
  def beFailing[T <% Result](message: String): Matcher[T] = beFailing(Some(message))
  def beFailing[T](message: Option[String])(implicit toResult: T => Result): Matcher[T] = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      result(toResult(value.value).isFailure,
             value.description + " is a failure",
             value.description + " is not a failure",
             value) and
      message.map(m=> result(value.value.message matches m,
                         value.value.message + " matches " + m,
                         value.value.message + " doesn't match " + m,
                         value)).getOrElse(result(true, "ok", "ko", value))
    }
  }

  def beError[T <% Result]: Matcher[T] = beError(None)
  def beError[T <% Result](message: String): Matcher[T] = beError(Some(message))
  def beError[T](message: Option[String])(implicit toResult: T => Result): Matcher[T] = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      result(toResult(value.value).isError,
             value.description + " is an error",
             value.description + " is not an error",
             value) and
      message.map(m=> result(value.value.message matches m,
                         value.value.message + " matches " + m,
                         value.value.message + " doesn't match " + m,
                         value)).getOrElse(result(true, "ok", "ko", value))
    }
  }

  def beSkipped[T <% Result]: Matcher[T] = beSkipped(None)
  def beSkipped[T <% Result](message: String): Matcher[T] = beSkipped(Some(message))
  def beSkipped[T](message: Option[String])(implicit toResult: T => Result): Matcher[T] = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      result(toResult(value.value).isSkipped,
             value.description + " is skipped",
             value.description + " is not skipped",
             value) and
      message.map(m=> result(value.value.message matches m,
                         value.value.message + " matches " + m,
                         value.value.message + " doesn't match " + m,
                         value)).getOrElse(result(true, "ok", "ko", value))
    }
  }
}
private[specs2]
trait ResultBeHaveMatchers { outer: ResultBaseMatchers =>
  implicit def toResultMatcher[T <% Result](result: MatchResult[T]) = new ResultMatcher(result)
  class ResultMatcher[T <% Result](result: MatchResult[T]) {
    def successful = result(outer.beSuccessful[T])
    def beSuccessful = result(outer.beSuccessful[T])

    def failing = result(outer.beFailing[T](".*"))
    def failing(m: String) = result(outer.beFailing[T](m))
    def beFailing = result(outer.beFailing[T](".*"))
    def beFailing(m: String) = result(outer.beFailing[T](m))
  }

  def successful = outer.beSuccessful[Result]
  def successful[T <% Result] = outer.beSuccessful[T]

  def failing = outer.beFailing[Result](".*")
  def failing[T <% Result] = outer.beFailing[T](".*")

  def failing[T <% Result](m: String = ".*") = outer.beFailing[T](".*")
}
