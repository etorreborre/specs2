package org.specs2
package specification
package core

import org.specs2.control._
import org.specs2.io.FileSystem
import org.specs2.io._
import org.specs2.main.Arguments
import org.specs2.reflect.ClassLoading
import org.specs2.reporter.PrinterLogger.NoPrinterLogger
import org.specs2.specification.process._

object EnvDefault {

  def default: Env =
    Env(
      arguments            = Arguments(),
      systemLogger         = ConsoleLogger(),
      printerLogger        = NoPrinterLogger,
      statisticsRepository = StatisticsRepositoryCreation.file(Arguments().commandLine.directoryOr("stats.outdir", "target" / "specs2-reports" / "stats")),
      random               = new scala.util.Random,
      fileSystem           = FileSystem(ConsoleLogger()),
      executionParameters  = ExecutionParameters(),
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
