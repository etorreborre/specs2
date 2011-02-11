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
  
  def beSuccessful[T <: Result] = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      result(value.value.isSuccess,
             value.description + " is a success",
             value.description + " is not a success",
             value)
    }
  }

  def beFailing[T <: Result]: Matcher[T] = beFailing(".*")
  def beFailing[T <: Result](message: String) = new Matcher[T] {
    def apply[S <: T](value: Expectable[S]) = {
      result(value.value.isFailure,
             value.description + " is a failure",
             value.description + " is not a failure",
             value) and
      result(value.value.message matches message,
             value.value.message + " matches " + message,
             value.value.message + " doesn't match " + message,
             value)
    }
  }
}
private[specs2]
trait ResultBeHaveMatchers { outer: ResultBaseMatchers =>
  implicit def toResultMatcher[T <: Result](result: MatchResult[T]) = new ResultMatcher(result)
  class ResultMatcher[T <: Result](result: MatchResult[T]) {
    def successful = result(outer.beSuccessful)
    def beSuccessful = result(outer.beSuccessful)

    def failing = result(outer.beFailing(".*"))
    def failing(m: String) = result(outer.beFailing(m))
    def beFailing = result(outer.beFailing(".*"))
    def beFailing(m: String) = result(outer.beFailing(m))
  }
  def successful = outer.beSuccessful

  def failing = outer.beFailing(".*")
  def failing(m: String = ".*") = outer.beFailing(".*")
}
