package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments

import scala.concurrent.ExecutionContext

/**
 * Execution environment for javascript
 */
case class ExecutionEnv(executorServices: ExecutorServices,
                        timeFactor: Int) {

  def shutdown(): Unit = ()

  lazy val executionContext = executorServices.executionContext
  lazy val scheduler = executorServices.scheduler

  implicit lazy val ec = executorServices.executionContext
}

object ExecutionEnv {

  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutionEnv =
    ExecutionEnv(
      ExecutorServices.fromExecutionContext(ec),
      timeFactor = 1)

  def create(arguments: Arguments, systemLogger: Logger): ExecutionEnv =
    fromGlobalExecutionContext

  def createSpecs2(arguments: Arguments, systemLogger: Logger): ExecutionEnv =
    fromGlobalExecutionContext

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}
