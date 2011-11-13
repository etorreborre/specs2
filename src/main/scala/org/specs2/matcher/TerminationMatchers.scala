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

class TerminationMatcher[-T](retries: Int, sleep: Duration, whenAction: Option[() => Any] = None, whenDesc: Option[String] = None, onlyWhen: Boolean = false) extends Matcher[T] {
  def apply[S <: T](a: Expectable[S]) =
    retry(retries, retries, sleep, a, promise(a.value))

  def retry[S <: T](originalRetries: Int, retries: Int, sleep: Duration, a: Expectable[S], promise: Promise[S]): MatchResult[S] = {
    Thread.sleep(sleep.inMillis)
    lazy val fulfilled = promise.fulfilled
    if (fulfilled || retries <= 0) {
      if (!fulfilled) promise.break

      val parameters = "with retries="+originalRetries+" and sleep="+sleep.inMillis
      if (onlyWhen && originalRetries >= 1) {
        val onlyWhenAction = whenDesc.getOrElse("the second action")
        result(false, "the action terminates", "the action terminates before "+onlyWhenAction+" ("+parameters+")", a)
      }
      else {
        val evenWhenAction = whenDesc.map(w => " even when "+w).getOrElse("")
        result(fulfilled, "the action terminates", "the action is blocking "+parameters+evenWhenAction, a)
      }
    }
    else if (retries == 1) {
      // try the action if defined, as a way to unblock the tested thread
      whenAction.map(_())
      retry(originalRetries, retries - 1, sleep, a, promise)
    } else
      retry(originalRetries, retries - 1, sleep, a, promise)
  }

  private def withAWhenAction[S](whenAction: Option[() => S], whenDesc: =>Option[String], onlyWhen: Boolean) =
    new TerminationMatcher(if (retries == 0) 1 else retries, sleep, whenAction, whenDesc, onlyWhen)

  def when[S](actionDescription: String, action: =>S): TerminationMatcher[T] =
    withAWhenAction(Some(() => action), Some(actionDescription), onlyWhen=false)


  def onlyWhen[S](actionDescription: String, action: =>S): TerminationMatcher[T] =
    withAWhenAction(Some(() => action), Some(actionDescription), onlyWhen=true)

  def when[S](action: =>S): TerminationMatcher[T] = withAWhenAction(Some(() => action), None, onlyWhen=false)

  def onlyWhen[S](action: =>S): TerminationMatcher[T] = withAWhenAction(Some(() => action), None, onlyWhen=true)
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