package org.specs2.matcher

import cats.effect.IO
import org.specs2.matcher.ValueChecks.valueIsTypedValueCheck
import org.specs2.matcher.describe.Diffable

import scala.concurrent.TimeoutException
import scala.concurrent.duration.FiniteDuration
import org.specs2.text.NotNullStrings._

trait RunTimedMatchers[F[_]] {

  protected def runWithTimeout[A](fa: F[A], timeout: FiniteDuration): A
  protected def runAwait[A](fa: F[A]) : A

  def returnOk[T]: TimedMatcher[T] =
    attemptRun(ValueCheck.alwaysOk, None)

  def returnValue[T](check: ValueCheck[T]): TimedMatcher[T] =
    attemptRun(check, None)

  def returnBefore[T](duration: FiniteDuration): TimedMatcher[T] =
    attemptRun(ValueCheck.alwaysOk, Some(duration))

  protected[specs2] def attemptRun[T](check: ValueCheck[T], duration: Option[FiniteDuration]): TimedMatcher[T] =
    new TimedMatcher(check, duration)

  class TimedMatcher[T](check: ValueCheck[T], duration: Option[FiniteDuration]) extends Matcher[F[T]] {

    def apply[S <: F[T]](e: Expectable[S]) = duration.fold(checkAwait(e))(checkWithDuration(e,_))

    def checkWithDuration[S <: F[T]](e: Expectable[S], d: FiniteDuration): MatchResult[S] =
      try {
        checkResult(e)(runWithTimeout(e.value, d))
      } catch {
        case x: TimeoutException => timeoutResult(e,d)
        case x: Exception => errorResult(e)(x)
      }

    def checkAwait[S <: F[T]](e: Expectable[S]) =
      try {
        checkResult(e)(runAwait(e.value))
      } catch {
        case x: Exception => errorResult(e)(x)
      }

    def before(d: FiniteDuration): TimedMatcher[T] =
      new TimedMatcher(check, Some(d))

    def withValue(check: ValueCheck[T]): TimedMatcher[T] =
      new TimedMatcher(check, duration)

    private def timeoutResult[S <: F[T]](e: Expectable[S], d: FiniteDuration): MatchResult[S] = {
      val message = s"Timeout after ${d.toMillis} milliseconds"
      result(false, message, message, e)
    }

    private def errorResult[S <: F[T]](e: Expectable[S])(t: Throwable): MatchResult[S] = {
      val message = "an exception was thrown "+t.getMessage.notNull+" "+t.getClass.getName
      result(false, message, message, e)
    }

    private def checkResult[S <: F[T]](e: Expectable[S])(t: T): MatchResult[S] =
      result(check.check(t), e)
  }

  // This withValue method cannot be set directly on the TimedMatcher class
  // otherwise it is always selected instead of the other withValue method
  implicit class TimedMatcherWithValue[T](timedMatcher: TimedMatcher[T]) {
    def withValue(t: T)(implicit di: Diffable[T]): TimedMatcher[T] =
      timedMatcher.withValue(valueIsTypedValueCheck(t))
  }
}

trait IOMatchers extends RunTimedMatchers[IO] {

  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val catsEffectTimer: cats.effect.Temporal[IO] =
    IO.timer(global)
  implicit val catsEffectContextShift: cats.effect.ContextShift[IO] =
    IO.contextShift(global)

  protected def runWithTimeout[A](fa: IO[A], d: FiniteDuration): A = fa.timeout(d).unsafeRunSync
  protected def runAwait[A](fa: IO[A]) : A = fa.unsafeRunSync

  protected[specs2] override def attemptRun[T](check: ValueCheck[T], duration: Option[FiniteDuration]): IOMatcher[T] =
    IOMatcher(check, duration)

  case class IOMatcher[T](check: ValueCheck[T], duration: Option[FiniteDuration])
      extends TimedMatcher[T](check, duration) {
    def checkIOWithDuration[S <: IO[T]](e: Expectable[S], d: FiniteDuration): MatchResult[S] =
      checkWithDuration(e, d)
    def checkIO[S <: IO[T]](e: Expectable[S]) = checkAwait(e)
  }

}
