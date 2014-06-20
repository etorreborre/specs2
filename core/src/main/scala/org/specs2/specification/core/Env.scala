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

case class Env(arguments: Arguments       = Arguments(),
               executionEnv: ExecutionEnv = ExecutionEnv(),
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

  /**
   * shutdown computing resources like thread pools
   */
  def shutdown = executionEnv.shutdown

  /**
   * @return an isolated env
   */
  def setWithoutIsolation = copy(executionEnv = executionEnv.setWithoutIsolation)

}

object Env {
  def executeResult[R : AsResult](r: Env => R) = {
    val env = Env()
    try AsResult(r(env))
    finally env.shutdown
  }
}