package org.specs2
package matcher

import AnyMatchers._
import DisjunctionMatchers._
import ValueChecks._
import org.specs2.matcher.describe.Diffable

import scala.reflect.ClassTag
import scalaz.concurrent.Task
import text.NotNullStrings._

/**
 * Matchers for scalaz.concurrent.Task
 */
trait TaskMatchers {

  def returnOk[T]: TaskMatcher[T] =
    attemptRun(ValueCheck.alwaysOk)

  def returnValue[T](check: ValueCheck[T]): TaskMatcher[T] =
    attemptRun(check)

  def failWith[T <: Throwable : ClassTag]: Matcher[Task[_]] =
    returnValue(be_-\/(haveClass[T])) ^^ { t: Task[_] => t.attempt }

  private[specs2] def attemptRun[T](check: ValueCheck[T]): TaskMatcher[T] =
    TaskMatcher(check)

  case class TaskMatcher[T](check: ValueCheck[T]) extends Matcher[Task[T]] {
    def apply[S <: Task[T]](e: Expectable[S]) =
      e.value.attemptRun.fold(failedAttempt(e), checkResult(e))

    def withValue(check: ValueCheck[T]): TaskMatcher[T] =
      TaskMatcher(check)

    def withValue(t: T)(implicit di: Diffable[T]): TaskMatcher[T] =
      withValue(valueIsTypedValueCheck(t))

    private def failedAttempt[S <: Task[T]](e: Expectable[S])(t: Throwable): MatchResult[S] = {
      val message = "an exception was thrown "+t.getMessage.notNull
      result(false, message, message, e)
    }

    private def checkResult[S <: Task[T]](e: Expectable[S])(t: T): MatchResult[S] =
      result(check.check(t), e)
  }
}

object TaskMatchers extends TaskMatchers
