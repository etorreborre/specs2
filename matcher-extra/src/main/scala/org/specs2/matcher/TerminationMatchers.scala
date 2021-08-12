package org.specs2
package matcher

import java.util.concurrent.atomic.AtomicBoolean
import concurrent.ExecutionEnv
import Matcher.{given}
import execute.*, Result.*

import scala.annotation.tailrec
import scala.concurrent.*, duration.*

/** This trait provides matchers to check if a block of code is terminating or not
  */
trait TerminationMatchers extends TerminationBaseMatchers
object TerminationMatchers extends TerminationMatchers

private[specs2] trait TerminationBaseMatchers:

  /** this matcher will check if a block of code terminates
    */
  def terminate[T](using ee: ExecutionEnv): TerminationMatcher[T] =
    new TerminationMatcher[T]()

  /** this matcher will check if a block of code terminates within a given duration, and a given number of retries
    */
  def terminate[T](retries: Int)(using ee: ExecutionEnv): TerminationMatcher[T] =
    new TerminationMatcher[T](retries)

  /** this matcher will check if a block of code terminates within a given duration, and a given number of retries
    */
  def terminate[T](sleep: Duration)(using ee: ExecutionEnv): TerminationMatcher[T] =
    new TerminationMatcher[T](sleep = sleep)

  /** this matcher will check if a block of code terminates within a given duration, and a given number of retries
    */
  def terminate[T](retries: Int, sleep: Duration)(using ee: ExecutionEnv): TerminationMatcher[T] =
    new TerminationMatcher[T](retries, sleep)

class TerminationMatcher[-T](
    retries: Int = 1,
    sleep: Duration = 100.millis,
    whenAction: Option[() => Any] = None,
    whenDesc: Option[String] = None,
    onlyWhen: Boolean = false
)(using ee: ExecutionEnv)
    extends Matcher[T]:

  def apply[S <: T](a: Expectable[S]) =
    retry(retries, retries, sleep * ee.timeFactor.toDouble, a, createFuture(a.value))

  @tailrec
  private final def retry[S <: T](
      originalRetries: Int,
      retries: Int,
      sleep: Duration,
      a: Expectable[S],
      future: Future[S],
      whenActionExecuted: Boolean = false
  ): Result =
    val parameters = "with retries=" + originalRetries + " and sleep=" + sleep.toMillis
    val evenWhenAction = whenDesc.fold("")(w => " even when " + w)
    val onlyWhenAction = whenDesc.getOrElse("the second action")

    def terminates =
      result(true, "the action is blocking " + parameters + evenWhenAction)

    def blocks =
      cancelled.set(true)
      result(false, "the action is blocking " + parameters + evenWhenAction)

    if whenAction.isDefined then
      if terminated.get then
        if onlyWhen then
          result(whenActionExecuted, "the action terminated before " + onlyWhenAction + " (" + parameters + ")")
        else terminates
      else if retries <= 0 then blocks
      else
        // leave the action a chance to finish
        Thread.sleep(sleep.toMillis)
        // if still not finished, try to execute the when action
        if !terminated.get && !whenActionExecuted then
          whenAction.map(_())
          // leave again the action a chance to finish
          Thread.sleep(sleep.toMillis)
          retry(originalRetries, retries - 1, sleep, a, future, whenActionExecuted = true)
        else retry(originalRetries, retries - 1, sleep, a, future)
    else if terminated.get then terminates
    else if retries <= 0 then blocks
    else
      Thread.sleep(sleep.toMillis)
      retry(originalRetries, retries - 1, sleep, a, future)

  /** use a variable to determine if the future has finished executing
    */
  private val terminated = new AtomicBoolean(false)
  private val cancelled = new AtomicBoolean(false)

  private def createFuture[A](a: =>A)(using ee: ExecutionEnv): Future[A] =
    val future = Future(a)(ee.executionContext)
    future.onComplete {
      case scala.util.Success(_) => terminated.set(true)
      case scala.util.Failure(_) => terminated.set(true)
    }(ee.executionContext)
    future

  private def withAWhenAction[S](whenAction: Option[() => S], whenDesc: =>Option[String], onlyWhen: Boolean) =
    new TerminationMatcher(retries, sleep, whenAction, whenDesc, onlyWhen)

  def when[S](actionDescription: String, action: =>S): TerminationMatcher[T] =
    withAWhenAction(Some(() => action), Some(actionDescription), onlyWhen = false)

  def onlyWhen[S](actionDescription: String, action: =>S): TerminationMatcher[T] =
    withAWhenAction(Some(() => action), Some(actionDescription), onlyWhen = true)

  def when[S](action: =>S): TerminationMatcher[T] = withAWhenAction(Some(() => action), None, onlyWhen = false)

  def onlyWhen[S](action: =>S): TerminationMatcher[T] = withAWhenAction(Some(() => action), None, onlyWhen = true)
