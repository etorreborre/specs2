package org.specs2.concurrent

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import org.scalajs.macrotaskexecutor.*

/** Executor services for javascript
  *
  * The global execution context is used for both executing tests and scheduling timeouts
  */
case class ExecutorServices(executionContextEval: () => ExecutionContext, schedulerEval: () => Scheduler) {

  given executionContext: ExecutionContext = executionContextEval()
  given scheduler: Scheduler = schedulerEval()

  def shutdownNow(): Unit =
    ()

  /** convenience method to shutdown the services when the final future has completed */
  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices =
    this

  def schedule(action: =>Unit, duration: FiniteDuration): Unit =
    // the timeout is started with a side effect
    // the return value, which is a handler to clear the timeout is ignored
    scheduler.schedule(action, duration)

}

object ExecutorServices {

  lazy val threadsNb: Int = 1
  lazy val specs2ThreadsNb: Int = 1

  def fromExecutionContext(ec: =>ExecutionContext): ExecutorServices =
    ExecutorServices(() => ec, () => Schedulers.default)

  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(MacrotaskExecutor)

}
