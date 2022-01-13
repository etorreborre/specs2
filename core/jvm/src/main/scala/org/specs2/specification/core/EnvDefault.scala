package org.specs2
package specification
package core

import org.specs2.control._
import org.specs2.io.FileSystem
import org.specs2.io._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.main.Arguments
import org.specs2.reflect.ClassLoading
import org.specs2.reporter._, LineLogger._
import org.specs2.specification.process._

object EnvDefault {

  def default: Env =
    create(Arguments(), consoleLogger)

  def create(arguments: Arguments, lineLogger: LineLogger): Env =
    Env(
      arguments = arguments,
      systemLogger = consoleLogging,
      selectorInstance    = (arguments: Arguments) => Arguments.instance(arguments.select.selector).getOrElse(DefaultSelector),
      executorInstance    = (arguments: Arguments) => Arguments.instance(arguments.execute.executor).getOrElse(DefaultExecutor),
      lineLogger          = lineLogger,
      statsRepository     = (arguments: Arguments) => StatisticsRepositoryCreation.file(arguments.commandLine.directoryOr("stats.outdir", "target" / "specs2-reports" / "stats")),
      random              = new scala.util.Random,
      fileSystem          = FileSystem,
      executionParameters = ExecutionParameters(),
      customClassLoader   = None,
      classLoading        = new ClassLoading {},
      executionEnv = ExecutionEnv.create(arguments, consoleLogging),
      specs2ExecutionEnv = ExecutionEnv.createSpecs2(arguments, consoleLogging)
    )

  def defaultInstances(env: Env) =
    List[AnyRef](env.arguments.commandLine,
         env.executionEnv,
         env.executionEnv.executorService,
         env.executionContext,
         env.arguments,
         env)

}
