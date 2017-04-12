package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments

import scala.concurrent.ExecutionContext

case class ExecutionEnv(executorServices: ExecutorServices,
                        timeFactor: Int) {

  def shutdown(): Unit =
    executorServices.shutdown.value

  lazy val executionContext         = executorServices.executionContext
  lazy val executorService          = executorServices.executorService
  lazy val scheduledExecutorService = executorServices.scheduledExecutorService
  lazy val scheduler                = executorServices.scheduler

  implicit lazy val es  = executorService
  implicit lazy val ses = scheduledExecutorService
  implicit lazy val ec  = executionContext

}

object ExecutionEnv {

  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutionEnv =
    ExecutionEnv(
      ExecutorServices.fromExecutionContext(ec),
      timeFactor = 1)

  def create(arguments: Arguments, systemLogger: Logger, threadFactoryName: String): ExecutionEnv =
    ExecutionEnv(
      ExecutorServices.create(arguments, systemLogger, threadFactoryName),
      timeFactor = arguments.execute.timeFactor)

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}
