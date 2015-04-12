package org.specs2
package specification
package core

import main.Arguments
import execute._
import reporter.LineLogger
import LineLogger._
import io._
import control._
import process.{Executor, DefaultExecutor, StatisticsRepository, Selector, DefaultSelector}
import scala.concurrent.ExecutionContext
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
case class Env(arguments: Arguments = Arguments(),
          /** selector class */
          selectorInstance: Arguments => Selector = (arguments: Arguments) =>
            Arguments.instance(arguments.select.selector).getOrElse(DefaultSelector),

          /** executor instance */
          executorInstance: Arguments => Executor = (arguments: Arguments) =>
            Arguments.instance(arguments.execute.executor).getOrElse(DefaultExecutor),

          /** default console logger */
          lineLogger: LineLogger = NoLineLogger,

          /** default statistics repository */
          statsRepository: Arguments => StatisticsRepository = (arguments: Arguments) =>
             StatisticsRepository.file(arguments.commandLine.directoryOr("stats.outdir", "target" / "specs2-reports" / "stats")),

          /** logger for issues */
          systemLogger: Logger = noLogging,

          /** random generator */
          random: scala.util.Random = new scala.util.Random,

          /** file system interface */
          fileSystem: FileSystem = FileSystem,

          /** parameters for fragments execution */
          executionParameters: ExecutionParameters = ExecutionParameters()) {

  lazy val statisticsRepository: StatisticsRepository =
    statsRepository(arguments)

  lazy val selector = selectorInstance(arguments)

  lazy val executor = executorInstance(arguments)

  /** execution environment */
  lazy val executionEnv: ExecutionEnv =
    ExecutionEnv.create(arguments, systemLogger, "env"+hashCode)

  lazy val executorService =
    executionEnv.executorService

  lazy val executionContext =
    executionEnv.executionContext

  lazy val scheduledExecutorService =
    executionEnv.scheduledExecutorService

  lazy val timeout =
    executionParameters.timeout

  def setTimeout(duration: FiniteDuration): Env =
    copy(executionParameters = executionParameters.setTimeout(duration))

  def shutdown(): Unit =
    executionEnv.shutdown()

  /** set new LineLogger */
  def setLineLogger(logger: LineLogger) =
    copy(lineLogger = logger)

  /** set new arguments */
  def setArguments(args: Arguments) =
    copy(arguments = args)

  /** @return an isolated env */
  def setWithoutIsolation =
    copy(executionParameters = executionParameters.setWithoutIsolation)

  /** set a new statistic repository */
  def setStatisticRepository(repository: StatisticsRepository) =
    copy(statsRepository = (args: Arguments) => repository)
}

object Env {
  def executeResult[R: AsResult](r: Env => R) = {
    val env = Env()
    AsResult(r(env))
  }
}

case class ExecutionParameters(
  timeout:  Option[FiniteDuration] = None,
  withoutIsolation: Boolean = false) {
  /**
   * fragments must not be created as "isolated"
   */
  def setWithoutIsolation: ExecutionParameters =
    copy(withoutIsolation = true)

  def setTimeout(duration: FiniteDuration): ExecutionParameters =
    copy(timeout = Some(duration))
}