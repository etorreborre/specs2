package org.specs2
package matcher

import scala.concurrent.duration._
import scalaz.stream._
import scalaz.concurrent._
import TaskMatchers._
import ValueCheck._

/**
 * Matchers for Process[Task, T]
 */
trait ProcessMatchers extends ExpectationsCreation {

  def returnValues[T](values: Seq[T]): ProcessMatcher[T] =
    ProcessMatcher(typedValueCheck(values), None)

  def returnValues[T](values: ValueCheck[Seq[T]]): ProcessMatcher[T] =
    ProcessMatcher(values, None)

  def returnLast[T](check: ValueCheck[T]): ProcessLastMatcher[T] =
    ProcessLastMatcher(toOptionCheck(check), None)

  def returnLastOption[T](value: Option[T]): ProcessLastMatcher[T] =
    ProcessLastMatcher(typedValueCheck(value), None)

  def returnLastOption[T](check: ValueCheck[Option[T]]): ProcessLastMatcher[T] =
    ProcessLastMatcher(check, None)

  def terminateBefore[T](duration: Duration): ProcessMatcher[T] =
    ProcessMatcher(ValueCheck.alwaysOk, Some(duration))

  case class ProcessMatcher[T](check: ValueCheck[Seq[T]], duration: Option[Duration]) extends Matcher[Process[Task, T]] {
    def apply[S <: Process[Task, T]](e: Expectable[S]) = {
      val process = e.value
      val r = attemptRun(check, duration).apply(createExpectable(process.runLog))
      result(r, e)
    }

    def before(d: Duration): ProcessMatcher[T] =
      copy(duration = Some(d))

    def withValues(values: Seq[T]): ProcessMatcher[T] =
      withValues(ValueChecks.valueIsTypedValueCheck(values))

    def withValues(check: ValueCheck[Seq[T]]): ProcessMatcher[T] =
      copy(check = check)

    def withLast(check: ValueCheck[Option[T]]): ProcessLastMatcher[T] =
      ProcessLastMatcher(check, duration)
  }

  case class ProcessLastMatcher[T](check: ValueCheck[Option[T]], duration: Option[Duration]) extends Matcher[Process[Task, T]] {
    def apply[S <: Process[Task, T]](e: Expectable[S]) = {
      val process = e.value
      result(attemptRun(check, duration).apply(createExpectable(process.runLast)), e)
    }

    def before(d: Duration): ProcessLastMatcher[T] =
      copy(duration = Some(d))

  }
}
