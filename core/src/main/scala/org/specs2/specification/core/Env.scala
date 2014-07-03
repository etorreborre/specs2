package org.specs2
package specification
package core

import main.Arguments
import org.specs2.execute.AsResult
import reporter.LineLogger
import LineLogger._
import io._
import control._
import process.StatisticsRepository

case class Env(arguments: Arguments           = Arguments(),
               indentationSize: Int           = 2,

               /** default console logger */
               lineLogger: LineLogger = NoLineLogger,

               /** default statistics repository */
               statsRepository: Arguments => StatisticsRepository = (arguments: Arguments) =>
                  StatisticsRepository.file(arguments.commandLine.value("stats.outdir").getOrElse("target/specs2-reports/stats")),

               /** execution environment */
               executionEnvironment: Arguments => ExecutionEnv = (arguments: Arguments) =>
                 ExecutionEnv(arguments),

               /** logger for issues */
               systemLogger: Logger = noLogging,

               /** random generator */
               random: scala.util.Random = new scala.util.Random,

               /** file system interface */
               fileSystem: FileSystem = FileSystem) {

  lazy val statisticsRepository: StatisticsRepository =
    statsRepository(arguments)


  lazy val executionEnv = executionEnvironment(arguments)

  /** shutdown computing resources like thread pools */
  def shutdown = executionEnv.shutdown

  /** @return an isolated env */
  def setWithoutIsolation =
    copy(executionEnvironment = (arguments: Arguments) => executionEnvironment(arguments).setWithoutIsolation)

  /** set a new statistic repository */
  def setStatisticRepository(repository: StatisticsRepository) =
    copy(statsRepository = (args: Arguments) => repository)

  /** set a new execution environment */
  def setExecutionEnv(env: ExecutionEnv) =
    copy(executionEnvironment = (args: Arguments) => env)
}

object Env {
  def apply(execEnv: ExecutionEnv) =
    new Env().setExecutionEnv(execEnv)

  def executeResult[R: AsResult](r: Env => R) = {
    val env = Env()
    try AsResult(r(env))
    finally env.shutdown
  }
}