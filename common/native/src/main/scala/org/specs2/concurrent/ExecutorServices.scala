package org.specs2.concurrent

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/** Executor services for Scala Native
  *
  * The global execution context is used for both executing tests and scheduling timeouts
  */
case class ExecutorServices(executionContextEval: () => ExecutionContext, schedulerEval: () => Scheduler):

  given executionContext: ExecutionContext = executionContextEval()
  given scheduler: Scheduler = schedulerEval()

  def shutdownNow(): Unit =
    ()

  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices =
    this

  def schedule(action: =>Unit, duration: FiniteDuration): Unit =
    scheduler.schedule(action, duration)

object ExecutorServices:

  lazy val threadsNb: Int = 1
  lazy val specs2ThreadsNb: Int = 1

  def fromExecutionContext(ec: =>ExecutionContext): ExecutorServices =
    ExecutorServices(() => ec, () => Schedulers.default)

  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(ExecutionContext.global)
