package org.specs2.matcher

import cats.effect.IO

import scala.concurrent.duration.FiniteDuration

trait IOMatchers extends RunTimedMatchers[IO] {

  import cats.effect.unsafe.implicits.global

  protected def runWithTimeout[A](fa: IO[A], d: FiniteDuration): A = fa.timeout(d).unsafeRunSync()
  protected def runAwait[A](fa: IO[A]) : A = fa.unsafeRunSync()

  protected[specs2] override def attemptRun[T](check: ValueCheck[T], duration: Option[FiniteDuration]): IOMatcher[T] =
    IOMatcher(check, duration)

  case class IOMatcher[T](check: ValueCheck[T], duration: Option[FiniteDuration])
      extends TimedMatcher[T](check, duration) {
    def checkIOWithDuration[S <: IO[T]](e: Expectable[S], d: FiniteDuration): MatchResult[S] =
      checkWithDuration(e, d)
    def checkIO[S <: IO[T]](e: Expectable[S]) = checkAwait(e)
  }

}
