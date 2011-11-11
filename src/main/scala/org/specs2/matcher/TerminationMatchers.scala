package org.specs2
package matcher

import time._
import TimeConversions._
import scala.actors._
import Futures._

/**
 * This trait provides matchers to check if a block of code is terminating or not
 */
trait TerminationMatchers {

  /**
   * this matchers will check if a block of code terminates within a given duration, after just one try
   */
  def terminate[T]: Matcher[T] = terminate()

  /**
   * this matchers will check if a block of code terminates within a given duration, and a given number of retries
   */
  def terminate[T](retries: Int = 0, sleep: Duration = 100.millis): Matcher[T] = new Matcher[T] {
    def apply[S <: T](a: Expectable[S]) =
      retry(retries, retries, sleep, a, future(a.value))

    def retry[S <: T](originalRetries: Int, retries: Int, sleep: Duration, a: Expectable[S], future: Future[S]): MatchResult[S] = {
      Thread.sleep(sleep.inMillis)
      lazy val isSet = future.isSet
      if (isSet || retries <= 0)
        result(isSet, "the action terminates", "the action is blocking with retries="+originalRetries+" and sleep="+sleep.inMillis, a)
      else
        retry(originalRetries, retries - 1, sleep, a, future)
    }
  }
}