package org.specs2.matcher

import cats.effect.IO
import org.specs2.execute._
import org.specs2.specification.core._

trait IOMatchers {
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val catsEffectTimer: cats.effect.Timer[IO] =
    IO.timer(global)
  implicit val catsEffectContextShift: cats.effect.ContextShift[IO] =
    IO.contextShift(global)

  implicit def ioAsExecution[R: AsResult]: AsExecution[IO[R]] = new AsExecution[IO[R]] {
    def execute(r: => IO[R]): Execution = Execution.withEnvAsync(_ => r.unsafeToFuture())
  }
}
