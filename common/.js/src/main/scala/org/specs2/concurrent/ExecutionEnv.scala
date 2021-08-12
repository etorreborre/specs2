package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments

import scala.concurrent.ExecutionContext

/** Execution environment for javascript
  */
case class ExecutionEnv(executorServices: ExecutorServices, timeFactor: Int):

  def shutdown(): Unit = ()

  given executionContext: ExecutionContext = executorServices.executionContext
  given ec: ExecutionContext = executorServices.executionContext

  lazy val scheduler = executorServices.scheduler

object ExecutionEnv:

  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutionEnv =
    ExecutionEnv(ExecutorServices.fromExecutionContext(ec), timeFactor = 1)

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    createSpecs2(arguments, systemLogger, tag)

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    fromGlobalExecutionContext

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)
