package org.specs2
package matcher

import time._
import TimeConversions._
import internal.scalaz._
import Scalaz._
import concurrent.Promise

/**
 * This trait provides matchers to check if a block of code is terminating or not
 */
trait TerminationMatchers extends TerminationBaseMatchers with TerminationNotMatchers

private[specs2]
trait TerminationBaseMatchers {

  /**
   * this matchers will check if a block of code terminates within a given duration, after just one try
   */
  def terminate[T]: TerminationMatcher[T] = terminate()

  /**
   * this matchers will check if a block of code terminates within a given duration, and a given number of retries
   */
  def terminate[T](retries: Int = 0, sleep: Duration = 100.millis) = new TerminationMatcher[T](retries, sleep)
}

class TerminationMatcher[-T](retries: Int, sleep: Duration, action: Option[() => Any] = None, when: Option[String] = None) extends Matcher[T] {
  def apply[S <: T](a: Expectable[S]) =
    retry(retries, retries, sleep, a, promise(a.value))

  def retry[S <: T](originalRetries: Int, retries: Int, sleep: Duration, a: Expectable[S], promise: Promise[S]): MatchResult[S] = {
    Thread.sleep(sleep.inMillis)
    lazy val fulfilled = promise.fulfilled
    if (fulfilled || retries <= 0) {
      if (!fulfilled) promise.break
      val evenWhenAction = when.map(w => " even when "+w).getOrElse("")
      result(fulfilled, "the action terminates", "the action is blocking with retries="+originalRetries+" and sleep="+sleep.inMillis+evenWhenAction, a)
    }
    else if (retries == 1) {
      // try the action if defined, as a way to unblock the tested thread
      action.map(_())
      retry(originalRetries, retries - 1, sleep, a, promise)
    } else
      retry(originalRetries, retries - 1, sleep, a, promise)
  }

  def when[S](actionDescription: String, action: =>S): TerminationMatcher[T] =
    new TerminationMatcher(retries + 1, sleep, Some(() => action), Some(actionDescription))

  def when[S](action: =>S): TerminationMatcher[T] =
    new TerminationMatcher(retries, sleep, Some(() => action), None)

}

/**
 * This trait adds the necessary implicit to write 'action must not terminate'
 */
private[specs2]
trait TerminationNotMatchers { outer: TerminationBaseMatchers =>

   implicit def toTerminationResultMatcher[T](result: MatchResult[T]) = new TerminationResultMatcher(result)

   class TerminationResultMatcher[T](result: MatchResult[T]) {
     def terminate = result(outer.terminate)
     def terminate(retries: Int = 0, sleep: Duration = 100.millis) = result(outer.terminate(retries, sleep))
   }
}