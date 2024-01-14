package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments

import scala.concurrent.*, duration.*

/** Execution environment
  */
case class ExecutionEnv(executorServices: ExecutorServices, timeFactor: Int):

  def shutdown(): Unit =
    executorServices.shutdownNow()

  given executionContext: ExecutionContext = executorServices.executionContext
  given ec: ExecutionContext = executorServices.executionContext

  lazy val scheduler = executorServices.scheduler

  def schedule(action: =>Unit, duration: FiniteDuration): Unit =
    executorServices.schedule(action, duration)

object ExecutionEnv extends ExecutionEnvCompanionPlatform
