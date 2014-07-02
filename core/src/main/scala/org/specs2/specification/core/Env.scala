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
               /** default statistics repository */
               statisticsRepository: StatisticsRepository = StatisticsRepository.file("target/specs2-reports/stats"),
               /** default console logger */
               lineLogger: LineLogger = NoLineLogger,
               /** logger for issues */
               systemLogger: Logger = noLogging,
               /** random generator */
               random: scala.util.Random = new scala.util.Random,
               /** file system interface */
               fileSystem: FileSystem = FileSystem) {

  lazy val executionEnv = ExecutionEnv(arguments)
  /**
   * shutdown computing resources like thread pools
   */
  def shutdown = executionEnv.shutdown

  /**
   * @return an isolated env
   */
  def setWithoutIsolation = new Env(arguments, indentationSize, statisticsRepository, lineLogger, systemLogger, random, fileSystem) {
    override lazy val executionEnv = ExecutionEnv(arguments).setWithoutIsolation
  }

}

object Env {
  def apply(executionEnvironment: ExecutionEnv) = new Env(executionEnvironment.arguments) {
    override lazy val executionEnv = executionEnvironment
  }

  def executeResult[R : AsResult](r: Env => R) = {
    val env = Env()
    try AsResult(r(env))
    finally env.shutdown
  }
}