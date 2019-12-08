package org.specs2
package specification
package core

import control._
import io.FileSystem
import main.Arguments
import reporter.PrinterLogger.NoPrinterLogger
import specification.process._
import reflect._

object EnvDefault {

  def default: Env =
    Env(
      arguments           = Arguments(),
      systemLogger        = ConsoleLogger(),
      printerLogger       = NoPrinterLogger,
      statsRepository     = (arguments: Arguments) => StatisticsRepositoryCreation.memory,
      random              = new scala.util.Random,
      fileSystem          = FileSystem(ConsoleLogger()),
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
