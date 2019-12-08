package org.specs2
package specification
package core

import org.specs2.control._
import org.specs2.io.FileSystem
import org.specs2.io._
import org.specs2.main.Arguments
import org.specs2.reflect.ClassLoading
import org.specs2.reporter.PrinterLogger.consolePrinterLogger
import org.specs2.specification.process._

object EnvDefault {

  def default: Env =
    create(Arguments())

  def create(arguments: Arguments): Env =
    Env(
      arguments            = arguments,
      systemLogger         = ConsoleLogger(),
      printerLogger        = consolePrinterLogger,
      statisticsRepository = StatisticsRepositoryCreation.file(arguments.commandLine.directoryOr("stats.outdir", "target" / "specs2-reports" / "stats")),
      random               = new scala.util.Random,
      fileSystem           = FileSystem(ConsoleLogger()),
      customClassLoader    = None,
      classLoading         = new ClassLoading {})

  def defaultInstances(env: Env) =
    List[AnyRef](env.arguments.commandLine,
         env.executionEnv,
         env.executionEnv.executorServices,
         env.executionContext,
         env.arguments,
         env)

}
