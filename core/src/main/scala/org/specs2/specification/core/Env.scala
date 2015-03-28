package org.specs2
package specification
package core

import main.Arguments
import org.specs2.execute.{ExecutionTimeFactor, AsResult}
import reporter.LineLogger
import LineLogger._
import io._
import control._
import process.{Executor, DefaultExecutor, StatisticsRepository, Selector, DefaultSelector}

import scala.concurrent.ExecutionContext

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

          /** execution environment */
          executionEnv: ExecutionEnv = ExecutionEnv(),

          /** logger for issues */
          systemLogger: Logger = noLogging,

          /** random generator */
          random: scala.util.Random = new scala.util.Random,

          /** file system interface */
          fileSystem: FileSystem = FileSystem) {

  lazy val statisticsRepository: StatisticsRepository =
    statsRepository(arguments)

  lazy val selector = selectorInstance(arguments)

  lazy val executor = executorInstance(arguments)

  lazy val executorService =
    ExecutionTimeFactor.decorateExecutorService(
      ExecutionEnv.executor(arguments.threadsNb, "env"+hashCode.toString),
      arguments.execute.timeFactor)

  lazy val executionContext =
    setTimeFactor(ExecutionContext.fromExecutorService(executorService,
    (t: Throwable) => control.logThrowable(t, arguments.verbose).execute(systemLogger).unsafePerformIO))

  def setTimeFactor(context: ExecutionContext): ExecutionContext =
    ExecutionTimeFactor.decorateExecutionContext(context, arguments.execute.timeFactor)


  lazy val timeout = (new Timeout).start

  def shutdown(): Unit = {
    try     executorService.shutdownNow
    finally timeout.stop()
  }

  /** set new LineLogger */
  def setLineLogger(logger: LineLogger) =
    copy(lineLogger = logger)

  /** set new arguments */
  def setArguments(args: Arguments) =
    copy(arguments = args)

  /** @return an isolated env */
  def setWithoutIsolation =
    copy(executionEnv = executionEnv.setWithoutIsolation)

  /** set a new statistic repository */
  def setStatisticRepository(repository: StatisticsRepository) =
    copy(statsRepository = (args: Arguments) => repository)

  /** set a new execution environment */
  def setExecutionEnv(env: ExecutionEnv) =
    copy(executionEnv = env)
}

object Env {
  def apply(execEnv: ExecutionEnv) =
    new Env().setExecutionEnv(execEnv)

  def executeResult[R: AsResult](r: Env => R) = {
    val env = Env()
    AsResult(r(env))
  }
}