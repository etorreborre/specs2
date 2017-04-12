package org.specs2.concurrent

import org.specs2.control.eff._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import org.specs2.main._
import org.specs2.control._

case class ExecutorServices(executionContextEval: Evaluated[ExecutionContext],
                            schedulerEval: Evaluated[Scheduler]) {

  implicit lazy val executionContext: ExecutionContext =
    executionContextEval.value

  implicit lazy val scheduler: Scheduler =
    schedulerEval.value

  /** convenience method to shutdown the services when the final future has completed */
  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices =
    this

  def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit =
    scheduler.schedule(timedout, duration)

}

object ExecutorServices {

  lazy val threadsNb: Int = 1

  def create(arguments: Arguments, systemLogger: Logger, threadFactoryName: String): ExecutorServices =
    fromGlobalExecutionContext

  def fromExecutionContext(ec: =>ExecutionContext): ExecutorServices =
    ExecutorServices(
      Memoized(ec),
      Memoized(Schedulers.default)
    )

  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}
