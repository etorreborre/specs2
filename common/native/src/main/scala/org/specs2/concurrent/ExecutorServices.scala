package org.specs2.concurrent

import org.specs2.control.eff._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/**
 * Executor services for javascript
 *
 * The global execution context is used for both
 * executing tests and scheduling timeouts
 */
case class ExecutorServices(executionContextEval: Evaluated[ExecutionContext],
                            schedulerEval: Evaluated[Scheduler]) {

  implicit lazy val executionContext: ExecutionContext =
    executionContextEval.value

  implicit lazy val scheduler: Scheduler =
    schedulerEval.value

  def shutdownNow(): Unit =
    ()

  /** convenience method to shutdown the services when the final future has completed */
  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices =
    this

  def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit =
    scheduler.schedule(timedout, duration)

}

object ExecutorServices {

  lazy val threadsNb: Int = 1
  lazy val specs2ThreadsNb: Int = 1

  def fromExecutionContext(ec: =>ExecutionContext): ExecutorServices =
    ExecutorServices(
      Memoized(ec),
      Memoized(Schedulers.default)
    )

  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}
