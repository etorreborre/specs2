package org.specs2
package specification
package core

import main.{Arguments, CommandLine}
import execute._
import concurrent.ExecutionEnv
import reporter.LineLogger
import io._
import control._
import process.{StatisticsRepository}
import reflect._

import scala.concurrent.duration.FiniteDuration

/**
 * Whole creation / execution / reporting environment for a specification
 *
 * Part of it is driven by the command-line, part of it is specs2 implementation
 * Yet it can be used to drive the creation or execution of examples
 *
 * Note: creating an Env instance is a delicate activity because the Env hold a thread pool that needs
 *       to be shutdown at the end of the execution
 */
case class Env(
  arguments:            Arguments,
  systemLogger:         Logger,
  lineLogger:           LineLogger,
  statisticsRepository: StatisticsRepository,
  random:               scala.util.Random,
  fileSystem:           FileSystem,
  executionParameters:  ExecutionParameters,
  customClassLoader:    Option[ClassLoader],
  classLoading:         ClassLoading,
  executionEnv:         ExecutionEnv,
  specs2ExecutionEnv:   ExecutionEnv) {

  lazy val executionContext =
    executionEnv.executionContext

  lazy val executorServices =
    executionEnv.executorServices

  lazy val specs2ExecutionContext =
    specs2ExecutionEnv.executionContext

  lazy val specs2ExecutorServices =
    specs2ExecutionEnv.executorServices

  lazy val timeout =
    executionParameters.timeout

  lazy val commandLine: CommandLine =
    arguments.commandLine

  lazy val defaultInstances =
    List[AnyRef](arguments.commandLine, executionEnv, executionContext, arguments, this)

  def setTimeout(duration: FiniteDuration): Env =
    copy(executionParameters = executionParameters.setTimeout(duration))

  def shutdown(): Unit =
    try     specs2ExecutionEnv.shutdown
    finally executionEnv.shutdown

  /** set new LineLogger */
  def setLineLogger(logger: LineLogger) =
    copy(lineLogger = logger)

  /** set new system logger */
  def setSystemLogger(logger: Logger) =
    copy(systemLogger = logger)

  /** set new arguments */
  def setArguments(args: Arguments) =
    copy(arguments = args)

  /** set a new statistic repository */
  def setStatisticRepository(repository: StatisticsRepository) =
    copy(statisticsRepository = repository)

  /** set a new classloader to be used as the context classloader for each execution */
  def setCustomClassLoader(classLoader: ClassLoader): Env =
    copy(customClassLoader = Some(classLoader))

  def setContextClassLoader(): Unit =
    customClassLoader.foreach(classLoading.setContextClassLoader)
}

object Env {

  def apply(
    arguments:            Arguments            = EnvDefault.default.arguments,
    systemLogger:         Logger               = EnvDefault.default.systemLogger,
    lineLogger:           LineLogger           = EnvDefault.default.lineLogger,
    statisticsRepository: StatisticsRepository = EnvDefault.default.statisticsRepository,
    random:               scala.util.Random    = EnvDefault.default.random,
    fileSystem:           FileSystem           = EnvDefault.default.fileSystem,
    executionParameters:  ExecutionParameters  = EnvDefault.default.executionParameters,
    customClassLoader:    Option[ClassLoader]  = EnvDefault.default.customClassLoader,
    classLoading:         ClassLoading         = EnvDefault.default.classLoading): Env =
    Env(
      arguments,
      systemLogger,
      lineLogger,
      statisticsRepository,
      random,
      fileSystem,
      executionParameters,
      customClassLoader,
      classLoading,
      executionEnv =       ExecutionEnv.create(arguments, systemLogger),
      specs2ExecutionEnv = ExecutionEnv.createSpecs2(arguments, systemLogger))

  def executeResult[R : AsResult](r: Env => R) = {
    lazy val env = Env()
    AsResult(r(env))
  }

}

case class ExecutionParameters(timeout: Option[FiniteDuration] = None) {
  def setTimeout(duration: FiniteDuration): ExecutionParameters =
    copy(timeout = Some(duration))
}
