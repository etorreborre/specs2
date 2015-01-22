package org.specs2
package matcher

import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.concurrent.duration._
import scalaz.concurrent.Future

/**
 * This trait provides matchers to check if a block of code is terminating or not
 */
trait TerminationMatchers extends TerminationBaseMatchers with TerminationNotMatchers
object TerminationMatchers extends TerminationMatchers

private[specs2]
trait TerminationBaseMatchers {

  /**
   * this matchers will check if a block of code terminates within a given duration, after just one try
   */
  def terminate[T](implicit es: ExecutorService): TerminationMatcher[T] = terminate()(es)

  /**
   * this matchers will check if a block of code terminates within a given duration, and a given number of retries
   */
  def terminate[T](retries: Int = 1, sleep: Duration = 100.millis)(implicit es: ExecutorService) = new TerminationMatcher[T](retries, sleep)
}

class TerminationMatcher[-T](retries: Int, sleep: Duration, whenAction: Option[() => Any] = None, whenDesc: Option[String] = None, onlyWhen: Boolean = false)(implicit es: ExecutorService) extends Matcher[T] {

  def apply[S <: T](a: Expectable[S]) =
    retry(retries, retries, sleep, a, createFuture(a.value))

  @tailrec
  private final def retry[S <: T](originalRetries: Int, retries: Int, sleep: Duration, a: Expectable[S], future: Future[S], whenActionExecuted: Boolean = false): MatchResult[S] = {
    val parameters = "with retries="+originalRetries+" and sleep="+sleep.toMillis
    val evenWhenAction = whenDesc.fold("")(w => " even when " + w)
    val onlyWhenAction = whenDesc.getOrElse("the second action")

    def terminates =
      result(true, "the action terminates", "the action is blocking "+parameters+evenWhenAction, a)

    def blocks = {
      cancelled.set(true)
      result(false, "the action terminates", "the action is blocking "+parameters+evenWhenAction, a)
    }

    if (whenAction.isDefined) {
      if (terminated.get) {
        if (onlyWhen) {
          result(whenActionExecuted,
                 "the action terminates only when "+onlyWhenAction+" terminates",
                 "the action terminated before "+onlyWhenAction+" ("+parameters+")", a)
        } else terminates
      } else {
        if (retries <= 0) blocks
        else {
          // leave the action a chance to finish
          Thread.sleep(sleep.toMillis)
          // if still not finished, try to execute the when action
          if (!terminated.get && !whenActionExecuted) {
            whenAction.map(_())
            // leave again the action a chance to finish
            Thread.sleep(sleep.toMillis)
            retry(originalRetries, retries - 1, sleep, a, future, whenActionExecuted = true)
          } else
            retry(originalRetries, retries - 1, sleep, a, future)
        }
      }
    } else {
      if (terminated.get) terminates
      else {
        if (retries <= 0) blocks
        else {
          Thread.sleep(sleep.toMillis)
          retry(originalRetries, retries - 1, sleep, a, future)
        }
      }
    }

  }

  /**
   * use a variable to determine if the future has finished executing
   */
  private var terminated = new AtomicBoolean(false)
  private var cancelled = new AtomicBoolean(false)
  private def createFuture[A](a: =>A)(implicit es: ExecutorService): Future[A] = {
    val future = Future(a)
    future.runAsyncInterruptibly(_ => terminated.set(true), cancelled)
    future
  }

  private def withAWhenAction[S](whenAction: Option[() => S], whenDesc: =>Option[String], onlyWhen: Boolean) =
    new TerminationMatcher(retries, sleep, whenAction, whenDesc, onlyWhen)

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

   implicit class TerminationResultMatcher[T](result: MatchResult[T])(implicit es: ExecutorService) {
     def terminate = result(outer.terminate)
     def terminate(retries: Int = 0, sleep: Duration = 100.millis) = result(outer.terminate(retries, sleep))
   }
}