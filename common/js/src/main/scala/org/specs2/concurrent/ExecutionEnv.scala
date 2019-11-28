package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments

import scala.concurrent.ExecutionContext

/**
 * Execution environment for javascript
 */
case class ExecutionEnv(executorServices: ExecutorServices, timeFactor: Int) {

  def shutdown(): Unit = ()

  lazy val executionContext: ExecutionContext =
    executorServices.executionContext
    
  implicit lazy val ec: ExecutionContext =
    executorServices.executionContext
}

object ExecutionEnv {

  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutionEnv =
    ExecutionEnv(ExecutorServices(() => ec, () => Schedulers.default), timeFactor = 1)

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    fromGlobalExecutionContext

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    fromGlobalExecutionContext

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}
