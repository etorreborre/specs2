package org.specs2
package specification
package core

import main.Arguments
import reporter.LineLogger
import LineLogger._
import io.FileSystem
import process.StatisticsRepository

case class Env(arguments: Arguments       = Arguments(),
               executionEnv: ExecutionEnv = ExecutionEnv(),
               indentationSize: Int           = 2,
               /** default statistics repository */
               statisticsRepository: StatisticsRepository = StatisticsRepository.file("target/specs2-reports/stats"),
               /** default console logger */
               lineLogger: LineLogger = NoLineLogger,
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
