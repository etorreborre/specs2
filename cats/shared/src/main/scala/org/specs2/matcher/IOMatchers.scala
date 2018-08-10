package org.specs2.matcher

import cats.effect.IO
import org.specs2.matcher.ValueChecks.valueIsTypedValueCheck
import org.specs2.matcher.describe.Diffable

import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration
import org.specs2.text.NotNullStrings._
import scala.concurrent.ExecutionContext.Implicits.global

trait IOMatchers {
  def returnOk[T]: IOMatcher[T] =
    attemptRun(ValueCheck.alwaysOk, None)

  def returnValue[T](check: ValueCheck[T]): IOMatcher[T] =
    attemptRun(check, None)

  def returnBefore[T](duration: FiniteDuration): IOMatcher[T] =
    attemptRun(ValueCheck.alwaysOk, Some(duration))

  private[specs2] def attemptRun[T](check: ValueCheck[T], duration: Option[FiniteDuration]): IOMatcher[T] =
    IOMatcher(check, duration)

  case class IOMatcher[T](check: ValueCheck[T], duration: Option[FiniteDuration]) extends Matcher[IO[T]] {
    def apply[S <: IO[T]](e: Expectable[S]) = duration.fold(checkIO(e))(checkIOWithDuration(e,_))

    def checkIOWithDuration[S <: IO[T]](e: Expectable[S], d: FiniteDuration): MatchResult[S] = {
      try {
        checkResult(e)(e.value.timeout(d).unsafeRunSync())
      } catch {
        case x: TimeoutException => timeoutResult(e,d)
        case x: Exception => errorResult(e)(x)
      }
    }

    def checkIO[S <: IO[T]](e: Expectable[S]) = {
      try {
        checkResult(e)(e.value.unsafeRunSync())
      } catch {
        case x: Exception => errorResult(e)(x)
      }
    }

    def before(d: FiniteDuration): IOMatcher[T] =
      copy(duration = Some(d))

    def withValue(check: ValueCheck[T]): IOMatcher[T] =
      copy(check = check)

    def withValue(t: T)(implicit di: Diffable[T]): IOMatcher[T] =
      withValue(valueIsTypedValueCheck(t))

    private def timeoutResult[S <: IO[T]](e: Expectable[S], d: FiniteDuration): MatchResult[S] = {
      val message = s"Timeout after ${d.toMillis} milliseconds"
      result(false, message, message, e)
    }

    private def errorResult[S <: IO[T]](e: Expectable[S])(t: Throwable): MatchResult[S] = {
      val message = "an exception was thrown "+t.getMessage.notNull+" "+t.getClass.getName
      result(false, message, message, e)
    }

    private def checkResult[S <: IO[T]](e: Expectable[S])(t: T): MatchResult[S] =
      result(check.check(t), e)
  }
}
