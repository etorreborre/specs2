package org.specs2
package concurrent

import org.specs2.execute.*
import scala.concurrent.*, duration.*

/** Execution environment for Scala Native
  */
case class ExecutionEnv(executorServices: ExecutorServices, timeFactor: Int):

  def shutdown(): Unit =
    ()

  def await(future: Future[Result], timeout: Duration = Duration.Inf): Result =
    Success()

  given executionContext: ExecutionContext = executorServices.executionContext
  given ec: ExecutionContext = executorServices.executionContext

  lazy val scheduler = executorServices.scheduler

  def schedule(action: =>Unit, duration: FiniteDuration): Unit =
    executorServices.schedule(action, duration)

object ExecutionEnv extends ExecutionEnvCompanionPlatform
