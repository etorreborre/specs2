package org.specs2
package specification
package core

import org.specs2.control.consoleLogging
import org.specs2.io.FileSystem
import org.specs2.io._
import org.specs2.main.Arguments
import org.specs2.reflect.ClassLoading
import org.specs2.reporter.LineLogger.NoLineLogger
import org.specs2.specification.process._

object EnvDefault {

  def default: Env =
    Env(
      arguments           = Arguments(),
      systemLogger        = consoleLogging,
      selectorInstance    = (arguments: Arguments) => Arguments.instance(arguments.select.selector).getOrElse(DefaultSelector),
      executorInstance    = (arguments: Arguments) => Arguments.instance(arguments.execute.executor).getOrElse(DefaultExecutor),
      lineLogger          = NoLineLogger,
      statsRepository     = (arguments: Arguments) => StatisticsRepositoryCreation.file(arguments.commandLine.directoryOr("stats.outdir", "target" / "specs2-reports" / "stats")),
      random              = new scala.util.Random,
      fileSystem          = FileSystem,
      executionParameters = ExecutionParameters(),
      customClassLoader   = None,
      classLoading        = new ClassLoading {})

  def defaultInstances(env: Env) =
    List[AnyRef](env.arguments.commandLine,
         env.executionEnv,
         env.executionEnv.executorService,
         env.executionContext,
         env.arguments,
         env)

}
