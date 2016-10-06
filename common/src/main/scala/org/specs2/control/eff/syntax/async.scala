package org.specs2.control.eff.syntax

import org.specs2.control.eff._

import scala.concurrent.Future
import scalaz.concurrent.Task

object async extends async

trait async {

  implicit class AsyncOps[A](e: Eff[Fx.fx1[Async], A]) {
    def runAsyncTask: Task[A] =
      AsyncInterpretation.runAsyncTask(e)

    def runAsyncFuture: Future[A] =
      AsyncInterpretation.runAsyncFuture(e)
  }

}

