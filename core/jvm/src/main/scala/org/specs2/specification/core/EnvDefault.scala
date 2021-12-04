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
import org.specs2.concurrent.*
import scala.collection.*

object EnvDefault:

  val statsDirectoryPath: DirectoryPath =
    "target" / "specs2-reports" / "stats"

  def default: Env =
    create(Arguments())

  def create(arguments: Arguments): Env =
    val systemLogger = ConsoleLogger()
    Env(
      arguments = arguments,
      resources = concurrent.TrieMap(),
      systemLogger = systemLogger,
      printerLogger = consolePrinterLogger,
      statisticsRepository =
        StatisticsRepositoryCreation.file(arguments.commandLine.directoryOr("stats.outdir", statsDirectoryPath)),
      random = new scala.util.Random,
      fileSystem = FileSystem(systemLogger),
      customClassLoader = None,
      classLoading = ClassLoading(),
      executionEnv = ExecutionEnv.create(arguments, systemLogger),
      specs2ExecutionEnv = ExecutionEnv.createSpecs2(arguments, systemLogger)
    )

  def defaultInstances(env: Env) =
    List[AnyRef](
      env.arguments.commandLine,
      env.executionEnv,
      env.executionEnv.executorServices,
      env.executionContext,
      env.arguments,
      env
    )
