package org.specs2
package specification
package core

import main.{Arguments, CommandLine}
import execute._
import concurrent.ExecutionEnv
import reporter.PrinterLogger
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
  /** arguments passed on the command line */
  arguments:            Arguments,

  /** specs2 logger to report the overall execution of a specification
   * including warnings and errors prior to the execution itself
   *
   * This way a specification run can be made verbose if necessary by using
   * this logger
   */
  systemLogger:         Logger,

  /** specification logger to print out the execution results */
  printerLogger:        PrinterLogger,

  /** the StatisticsRepository contains the result of previous executions
   * in case they are needed to drive the current execution */
  statisticsRepository: StatisticsRepository,

  /** the StatisticsRepository contains the result of previous executions
   * in case they are needed to drive the current execution */
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

  /** set new PrinterLogger */
  def setPrinterLogger(logger: PrinterLogger) =
    copy(printerLogger = logger)

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
    printerLogger:           PrinterLogger           = EnvDefault.default.printerLogger,
    statisticsRepository: StatisticsRepository = EnvDefault.default.statisticsRepository,
    random:               scala.util.Random    = EnvDefault.default.random,
    fileSystem:           FileSystem           = EnvDefault.default.fileSystem,
    executionParameters:  ExecutionParameters  = EnvDefault.default.executionParameters,
    customClassLoader:    Option[ClassLoader]  = EnvDefault.default.customClassLoader,
    classLoading:         ClassLoading         = EnvDefault.default.classLoading): Env =
    Env(
      arguments,
      systemLogger,
      printerLogger,
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
