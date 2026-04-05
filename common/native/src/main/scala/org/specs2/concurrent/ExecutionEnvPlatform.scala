package org.specs2
package concurrent

import org.specs2.control.Logger
import org.specs2.main.Arguments
import scala.concurrent.*

private[concurrent] trait ExecutionEnvCompanionPlatform:

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    createSpecs2(arguments, systemLogger, tag)

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutionEnv =
    fromGlobalExecutionContext

  def fromGlobalExecutionContext: ExecutionEnv =
    ExecutionEnv(ExecutorServices.fromGlobalExecutionContext, timeFactor = 1)
