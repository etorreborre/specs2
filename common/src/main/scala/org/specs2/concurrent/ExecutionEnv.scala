package org.specs2
package concurrent

import org.specs2.main.Arguments
import org.specs2.control.Logger
import java.util.concurrent.*
import scala.concurrent.ExecutionContext

case class ExecutionEnv(executorServices: ExecutorServices, timeFactor: Int):

  lazy val executionContext         = executorServices.executionContext
  lazy val executorService: ExecutorService          = executorServices.executorService
  lazy val scheduledExecutorService = executorServices.scheduledExecutorService
  lazy val scheduler                = executorServices.scheduler

  given es: ExecutorService = executorService
  given ses: ScheduledExecutorService = scheduledExecutorService
  given ec: ExecutionContext = executionContext

  def shutdown(): Unit =
    executorServices.shutdownNow()

  def setTimeFactor(tf: Int): ExecutionEnv =
    copy(timeFactor = tf)

  def isShutdown: Boolean =
    executorService.isShutdown

object ExecutionEnv:

  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutionEnv =
    ExecutionEnv(
      ExecutorServices.fromExecutionContext(ec),
      timeFactor = 1)

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    ExecutionEnv(
      ExecutorServices.create(arguments, systemLogger, tag),
      timeFactor = arguments.execute.timeFactor)

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    ExecutionEnv(
      ExecutorServices.createSpecs2(arguments, systemLogger, tag),
      timeFactor = arguments.execute.timeFactor)

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)
