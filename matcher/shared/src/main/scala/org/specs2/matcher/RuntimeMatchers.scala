package org.specs2
package matcher

import ValueChecks.valueIsTypedValueCheck
import describe.Diffable

import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration
import text.NotNullStrings.*
import execute.*, Result.*

/** This trait provides matchers for IO-like datatypes which can be executed given an execution environment.
  *
  * For example you can write with cats.effect
  *
  * `"adding 1 + 1" ! IO(add(1, 1)) must returnValue(2)`
  *
  * However this example will be executed synchronously so those matchers are better used with test libraries which
  * don't support asynchronous execution (like JUnit).
  *
  * A better approach is to provide an implicit, like the one provided in specs2-cats to convert an IO value into a
  * specs2 Execution. Then you can directly write:
  *
  * `"adding 1 + 1" ! IO(add(1, 1) === 2)`
  */
trait RunTimedMatchers[F[_]]:

  protected def runWithTimeout[A](fa: F[A], timeout: FiniteDuration): A
  protected def runAwait[A](fa: F[A]): A

  /** succeed if a value is returned */
  def returnOk[T]: TimedMatcher[T] =
    attemptRun(ValueCheck.alwaysOk, None)

  /** succeed if a value satisfying the ValueCheck is returned */
  def returnValue[T](check: ValueCheck[T]): TimedMatcher[T] =
    attemptRun(check, None)

  /** succeed if a value is returned before a given time */
  def returnBefore[T](duration: FiniteDuration): TimedMatcher[T] =
    attemptRun(ValueCheck.alwaysOk, Some(duration))

  protected[specs2] def attemptRun[T](check: ValueCheck[T], duration: Option[FiniteDuration]): TimedMatcher[T] =
    new TimedMatcher(check, duration)

  /** This Matchers supports combinations of value + duration checks */
  open class TimedMatcher[T](check: ValueCheck[T], duration: Option[FiniteDuration]) extends Matcher[F[T]]:

    def apply[S <: F[T]](e: Expectable[S]): Result =
      duration.fold(checkAwait(e.value))(checkWithDuration(e.value, _))

    def checkWithDuration(value: F[T], d: FiniteDuration): Result =
      try check.check(runWithTimeout(value, d))
      catch {
        case x: TimeoutException => timeoutResult(d)
        case x: Exception        => errorResult(x)
      }

    def checkAwait(value: F[T]): Result =
      try check.check(runAwait(value))
      catch { case x: Exception => errorResult(x) }

    def before(d: FiniteDuration): TimedMatcher[T] =
      new TimedMatcher(check, Some(d))

    def withValue(check: ValueCheck[T]): TimedMatcher[T] =
      new TimedMatcher(check, duration)

    private def timeoutResult(d: FiniteDuration): Result =
      val message = s"Timeout after ${d.toMillis} milliseconds"
      result(false, message)

    private def errorResult(t: Throwable): Result =
      val message = "an exception was thrown " + t.getMessage.notNull + " " + t.getClass.getName
      result(false, message)

  // This withValue method cannot be set directly on the TimedMatcher class
  // otherwise it is always selected instead of the other withValue method
  extension [T](timedMatcher: TimedMatcher[T])
    def withValue(t: T)(using di: Diffable[T]): TimedMatcher[T] =
      timedMatcher.withValue(valueIsTypedValueCheck(t))
