package org.specs2
package specification
package core

import control._
import io.FileSystem
import main.Arguments
import reporter._, LineLogger._
import specification.process._
import concurrent._
import reflect._

object EnvDefault {

  def default: Env =
    create(Arguments(), consoleLogger)

  def create(arguments: Arguments, lineLogger: LineLogger): Env =
    Env(
      arguments           = arguments,
      systemLogger        = consoleLogging,
      selectorInstance    = (arguments: Arguments) => DefaultSelector,
      executorInstance    = (arguments: Arguments) => DefaultExecutor,
      lineLogger          = lineLogger,
      statsRepository     = (arguments: Arguments) => StatisticsRepositoryCreation.memory,
      random              = new scala.util.Random,
      fileSystem          = new FileSystem {},
      executionParameters = ExecutionParameters(),
      customClassLoader   = None,
      classLoading        = new ClassLoading {},
      executionEnv        = ExecutionEnv.create(arguments, consoleLogging),
      specs2ExecutionEnv  = ExecutionEnv.createSpecs2(arguments, consoleLogging))

  def defaultInstances(env: Env) =
    List[AnyRef](
      env.arguments.commandLine,
      env.executionEnv,
      env.executionContext,
      env.arguments,
      env)

}
