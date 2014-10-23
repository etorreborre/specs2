package org.specs2
package matcher

import execute._
import TaskMatchers._
import scalaz.stream._
import scalaz.concurrent._
import matcher.ValueCheck._

/**
 * Matchers for Process[Task, T]
 */
trait ProcessMatchers extends Expectations {

  def returnValues[T](values: Seq[T]): ProcessMatcher[T] =
    ProcessMatcher(typedValueCheck(values))

  def returnValues[T](values: ValueCheck[Seq[T]]): ProcessMatcher[T] =
    ProcessMatcher(values)

  def returnLast[T](check: ValueCheck[T]): ProcessLastMatcher[T] =
    ProcessLastMatcher(toOptionCheck(check))

  def returnLastOption[T](value: Option[T]): ProcessLastMatcher[T] =
    ProcessLastMatcher(typedValueCheck(value))

  def returnLastOption[T](check: ValueCheck[Option[T]]): ProcessLastMatcher[T] =
    ProcessLastMatcher(check)

  case class ProcessMatcher[T](check: ValueCheck[Seq[T]]) extends Matcher[Process[Task, T]] {
    def apply[S <: Process[Task, T]](e: Expectable[S]) = {
      val process = e.value
      val r = attemptRun(check).apply(createExpectable(process.runLog))
      result(r, e)
    }

    def withValues(values: Seq[T]): ProcessMatcher[T] =
      withValues(ValueChecks.valueIsTypedValueCheck(values))

    def withValues(check: ValueCheck[Seq[T]]): ProcessMatcher[T] =
      copy(check = check)

    def withLast(check: ValueCheck[Option[T]]): ProcessLastMatcher[T] =
      ProcessLastMatcher(check)
  }

  case class ProcessLastMatcher[T](check: ValueCheck[Option[T]]) extends Matcher[Process[Task, T]] {
    def apply[S <: Process[Task, T]](e: Expectable[S]) = {
      val process = e.value
      result(attemptRun(check).apply(createExpectable(process.runLast)), e)
    }
  }
}

