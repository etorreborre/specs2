package org.specs2.matcher

import cats.effect.IO
import org.specs2.execute._
import org.specs2.matcher.ValueChecks.valueIsTypedValueCheck
import org.specs2.matcher.describe.Diffable
import org.specs2.specification.core._
import org.specs2.text.NotNullStrings._

trait RunFMatchers[F[_]] {

  protected def runAwait[A](fa: F[A]) : A

  def returnOk[T]: FMatcher[T] =
    attemptRun(ValueCheck.alwaysOk)

  def returnValue[T](check: ValueCheck[T]): FMatcher[T] =
    attemptRun(check)

  protected[specs2] def attemptRun[T](check: ValueCheck[T]): FMatcher[T] =
    new FMatcher(check)

  class FMatcher[T](check: ValueCheck[T]) extends Matcher[F[T]] {

    def apply[S <: F[T]](e: Expectable[S]) = checkAwait(e)

    def checkAwait[S <: F[T]](e: Expectable[S]) =
      try {
        checkResult(e)(runAwait(e.value))
      } catch {
        case x: Exception => errorResult(e)(x)
      }

    def withValue(check: ValueCheck[T]): FMatcher[T] =
      new FMatcher(check)

    def withValue(t: T)(implicit di: Diffable[T]): FMatcher[T] =
      withValue(valueIsTypedValueCheck(t))

    private def errorResult[S <: F[T]](e: Expectable[S])(t: Throwable): MatchResult[S] = {
      val message = "an exception was thrown "+t.getMessage.notNull+" "+t.getClass.getName
      result(false, message, message, e)
    }

    private def checkResult[S <: F[T]](e: Expectable[S])(t: T): MatchResult[S] =
      result(check.check(t), e)
  }
}

trait IOMatchers extends RunFMatchers[IO] {
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val catsEffectTimer: cats.effect.Timer[IO] =
    IO.timer(global)
  implicit val catsEffectContextShift: cats.effect.ContextShift[IO] =
    IO.contextShift(global)

  implicit def ioAsExecution[R: AsResult]: AsExecution[IO[R]] = new AsExecution[IO[R]] {
    def execute(r: => IO[R]): Execution = Execution.withEnvAsync(_ => r.unsafeToFuture())
  }

  override protected def runAwait[A](fa: IO[A]) : A = fa.unsafeRunSync

  protected[specs2] override def attemptRun[T](check: ValueCheck[T]): IOMatcher[T] =
    IOMatcher(check)

  case class IOMatcher[T](check: ValueCheck[T]) extends FMatcher[T](check) {
    def checkIO[S <: IO[T]](e: Expectable[S]) = checkAwait(e)
  }

}
