package org.specs2
package specification
package core

import control.*
import io.FileSystem
import main.Arguments
import reporter.PrinterLogger.NoPrinterLogger
import specification.process.*
import reflect.*
import scala.collection.mutable.{Map as MutableMap}
object EnvDefault {

  def default: Env =
    create(Arguments())

  def create(arguments: Arguments): Env =
    Env(
      arguments            = arguments,
      resources            = MutableMap(),
      systemLogger         = ConsoleLogger(),
      printerLogger        = NoPrinterLogger,
      statisticsRepository = StatisticsRepositoryCreation.memory,
      random               = new scala.util.Random,
      fileSystem           = FileSystem(ConsoleLogger()),
      customClassLoader    = None,
      classLoading         = new ClassLoading {}
    )

  def defaultInstances(env: Env) =
    List[AnyRef](
      env.arguments.commandLine,
      env.executionEnv,
      env.executionContext,
      env.arguments,
      env)

}
