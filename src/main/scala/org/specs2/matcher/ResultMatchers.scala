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
}
private[specs2]
trait ResultBeHaveMatchers { outer: ResultBaseMatchers =>
  implicit def toResultMatcher[T <: Result](result: MatchResult[T]) = new ResultMatcher(result)
  class ResultMatcher[T <: Result](result: MatchResult[T]) {
    def successful = result(outer.beSuccessful)
    def beSuccessful = result(outer.beSuccessful)
  }
  def successful = outer.beSuccessful
}
