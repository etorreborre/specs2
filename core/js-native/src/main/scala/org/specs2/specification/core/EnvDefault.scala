package org.specs2
package specification
package core

import control._
import io.FileSystem
import main.Arguments
import reporter.LineLogger.NoLineLogger
import specification.process._
import reflect._

object EnvDefault {

  def default: Env =
    Env(
      arguments           = Arguments(),
      systemLogger        = consoleLogging,
      selectorInstance    = (arguments: Arguments) => DefaultSelector,
      executorInstance    = (arguments: Arguments) => DefaultExecutor,
      lineLogger          = NoLineLogger,
      statsRepository     = (arguments: Arguments) => StatisticsRepositoryCreation.memory,
      random              = new scala.util.Random,
      fileSystem          = new FileSystem {},
      executionParameters = ExecutionParameters(),
      customClassLoader   = None,
      classLoading        = new ClassLoading {}
    )

  def defaultInstances(env: Env) =
    List[AnyRef](
      env.arguments.commandLine,
      env.executionEnv,
      env.executionContext,
      env.arguments,
      env)

}
