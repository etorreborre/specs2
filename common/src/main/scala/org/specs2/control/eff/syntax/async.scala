package org.specs2.control.eff.syntax

import org.specs2.control.eff._

import scala.concurrent._
import scalaz._

object async extends async

trait async {

  implicit class AsyncFinalOps[A](e: Eff[Fx.fx1[Async], A]) {
    def runAsyncFuture: Future[A] =
      AsyncInterpretation.runAsyncFuture(e)
  }

  implicit class AsyncOps[R, A](e: Eff[R, A]) {
    def attempt(implicit task: Async /= R): Eff[R, Throwable \/ A] =
      AsyncInterpretation.attempt(e)
  }

}

