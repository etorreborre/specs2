package org.specs2
package matcher

import execute._
import MatchResultLogicalCombinators._

/**
 * Matchers for Results
 */
trait ResultMatchers extends ResultBaseMatchers with ResultBeHaveMatchers
object ResultMatchers extends ResultMatchers

private[specs2]
trait ResultBaseMatchers {
  
  def beSuccessful[T : AsResult] = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      result(AsResult[T](value.value).isSuccess,
             value.description + " is a success",
             value.description + " is not a success",
             value)
    }
  }

  def beFailing[T : AsResult]: Matcher[T] = beFailing(None)
  def beFailing[T : AsResult](message: String): Matcher[T] = beFailing(Some(message))
  def beFailing[T : AsResult](message: Option[String]): Matcher[T] = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      val r = AsResult[T](value.value)
      result(r.isFailure,
             value.description + " is a failure",
             value.description + " is not a failure",
             value) and
      message.map(m=> result(r.message matches m,
                             r.message + " matches " + m,
                             r.message + " doesn't match " + m,
                             value)).getOrElse(result(true, "ok", "ko", value))
    }
  }

  def beError[T : AsResult]: Matcher[T] = beError(None)
  def beError[T : AsResult](message: String): Matcher[T] = beError(Some(message))
  def beError[T : AsResult](message: Option[String]): Matcher[T] = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      val r = AsResult[T](value.value)
      result(r.isError,
             value.description + " is an error",
             value.description + " is not an error",
             value) and
      message.map(m=> result(r.message matches m,
                             r.message + " matches " + m,
                             r.message + " doesn't match " + m,
                             value)).getOrElse(result(true, "ok", "ko", value))
    }
  }

  def beSkipped[T : AsResult]: Matcher[T] = beSkipped(None)
  def beSkipped[T : AsResult](message: String): Matcher[T] = beSkipped(Some(message))
  def beSkipped[T : AsResult](message: Option[String]): Matcher[T] = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      val r = AsResult[T](value.value)
      result(r.isSkipped,
             value.description + " is skipped",
             value.description + " is not skipped",
             value) and
      message.map(m=> result(r.message matches m,
                             r.message + " matches " + m,
                             r.message + " doesn't match " + m,
                             value)).getOrElse(result(true, "ok", "ko", value))
    }
  }
}
private[specs2]
trait ResultBeHaveMatchers { outer: ResultBaseMatchers =>
  implicit def toResultMatcher[T : AsResult](result: MatchResult[T]) = new ResultMatcher(result)
  class ResultMatcher[T : AsResult](result: MatchResult[T]) {
    def successful = result(outer.beSuccessful[T])
    def beSuccessful = result(outer.beSuccessful[T])

    def failing = result(outer.beFailing[T](".*"))
    def failing(m: String) = result(outer.beFailing[T](m))
    def beFailing = result(outer.beFailing[T](".*"))
    def beFailing(m: String) = result(outer.beFailing[T](m))
  }

  def successful = outer.beSuccessful[Result]
  def successful[T : AsResult] = outer.beSuccessful[T]

  def failing = outer.beFailing[Result](".*")
  def failing[T : AsResult] = outer.beFailing[T](".*")

  def failing[T : AsResult](m: String = ".*") = outer.beFailing[T](".*")
}
