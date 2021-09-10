package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments
import scala.concurrent.*, duration.*

private[concurrent] trait ExecutionEnvCompanionPlatform:

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    createSpecs2(arguments, systemLogger, tag)

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    fromGlobalExecutionContext

  /** create an ExecutionEnv from the MacrotaskExecutor, an ExecutionContext which truly works with timeouts */
  def fromGlobalExecutionContext: ExecutionEnv =
    ExecutionEnv(ExecutorServices.fromGlobalExecutionContext, timeFactor = 1)
