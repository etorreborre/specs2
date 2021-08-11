package org.specs2
package specification
package core

import org.specs2.control.*
import org.specs2.io.FileSystem
import org.specs2.io.*
import org.specs2.io.FileName.*
import org.specs2.main.Arguments
import org.specs2.reflect.ClassLoading
import org.specs2.reporter.PrinterLogger.consolePrinterLogger
import org.specs2.specification.process.*
import scala.collection.mutable.{Map as MutableMap}

object EnvDefault:

  def default: Env =
    create(Arguments())

  def create(arguments: Arguments): Env =
    Env(
      arguments            = arguments,
      resources            = MutableMap(),
      systemLogger         = ConsoleLogger(),
      printerLogger        = consolePrinterLogger,
      statisticsRepository = StatisticsRepositoryCreation.memory,
      random               = new scala.util.Random,
      fileSystem           = FileSystem(ConsoleLogger()),
      customClassLoader    = None,
      classLoading         = ClassLoading())

  def defaultInstances(env: Env) =
    List[AnyRef](env.arguments.commandLine,
         env.executionEnv,
         env.executionEnv.executorServices,
         env.executionContext,
         env.arguments,
         env)
