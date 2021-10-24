package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments
import scala.concurrent._
import java.util.concurrent._

case class ExecutionEnv(executorServices: ExecutorServices,
                        timeFactor: Int,
                        retriesFactor: Int) {

  def shutdown(): Unit =
    executorServices.shutdownNow()

  lazy val executionContext: ExecutionContext =
    executorServices.executionContext

  lazy val executorService: ExecutorService =
    executorServices.executorService

  lazy val scheduledExecutorService: ScheduledExecutorService =
    executorServices.scheduledExecutorService

  lazy val scheduler: Scheduler =
    executorServices.scheduler

  implicit lazy val es: ExecutorService  =
    executorService

  implicit lazy val ses: ScheduledExecutorService =
    scheduledExecutorService

  implicit lazy val ec: ExecutionContext =
    executionContext

  def setTimeFactor(tf: Int): ExecutionEnv =
    copy(timeFactor = tf)

  def setRetriesFactor(tf: Int): ExecutionEnv =
    copy(retriesFactor = tf)

  def isShutdown: Boolean =
    executorService.isShutdown
}

object ExecutionEnv {

  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutionEnv =
    ExecutionEnv(
      ExecutorServices.fromExecutionContext(ec),
      timeFactor = 1,
      retriesFactor = 1)

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv = {
    ExecutionEnv(
      ExecutorServices.create(arguments, systemLogger, tag),
      timeFactor = arguments.execute.timeFactor,
      retriesFactor = arguments.execute.retriesFactor)
  }

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv = {
    ExecutionEnv(
      ExecutorServices.createSpecs2(arguments, systemLogger, tag),
      timeFactor = arguments.execute.timeFactor,
      retriesFactor = arguments.execute.retriesFactor)
  }

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}
