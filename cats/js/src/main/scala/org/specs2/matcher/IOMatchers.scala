package org.specs2.matcher

import cats.effect._
import cats.implicits._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.execute._
import org.specs2.specification.core._

trait IOMatchers {
  implicit val ee: ExecutionEnv
  implicit def catsEffectTimer(implicit ee: ExecutionEnv): cats.effect.Timer[IO] =
    IO.timer(ee.ec)
  implicit def catsEffectContextShift(implicit ee: ExecutionEnv): cats.effect.ContextShift[IO] =
    IO.contextShift(ee.ec)

  implicit def ioAsExecution[R: AsResult]: AsExecution[IO[R]] = new AsExecution[IO[R]] {
    def execute(r: => IO[R]): Execution = Execution.withEnvAsync(env => (IO.shift(env.executionContext) >> r).unsafeToFuture())
  }
}
