package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments

import scala.concurrent.ExecutionContext

private[concurrent] trait ExecutionEnvCompanionPlatform:

  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutionEnv =
    ExecutionEnv(ExecutorServices.fromExecutionContext(ec), timeFactor = 1)

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    ExecutionEnv(ExecutorServices.create(arguments, systemLogger, tag), arguments.execute.timeFactor)

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    // ExecutionEnv(ExecutorServices.createSpecs2(arguments, systemLogger, tag), arguments.execute.timeFactor)
    fromGlobalExecutionContext

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)
