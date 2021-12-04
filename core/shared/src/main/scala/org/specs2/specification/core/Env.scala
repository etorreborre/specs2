package org.specs2
package specification
package core

import main.{Arguments, CommandLine}
import execute.*
import concurrent.ExecutionEnv
import reporter.PrinterLogger
import io.*
import fp.*, syntax.*
import control.*
import process.{StatisticsRepository}
import reflect.*
import scala.collection.*
import scala.concurrent.*, duration.*

/** Whole creation / execution / reporting environment for a specification
  *
  * Part of it is driven by the command-line, part of it is specs2 implementation Yet it can be used to drive the
  * creation or execution of examples
  *
  * Note: creating an Env instance is a delicate activity because the Env hold a thread pool that needs to be shutdown
  * at the end of the execution
  */
case class Env(
    /** arguments passed on the command line */
    arguments: Arguments,
    /** dynamically (and untyped!) acquired resources, by resource key */
    resources: Resources,
    /** specs2 logger to report the overall execution of a specification including warnings and errors prior to the
      * execution itself
      *
      * This way a specification run can be made verbose if necessary by using this logger
      */
    systemLogger: Logger,
    /** specification logger to print out the execution results */
    printerLogger: PrinterLogger,
    /** the StatisticsRepository contains the result of previous executions in case they are needed to drive the current
      * execution
      */
    statisticsRepository: StatisticsRepository,
    /** the random object is only invoked when using the RandomSequentialExecutor to execute examples randomly in a
      * specification
      */
    random: scala.util.Random,
    /** the FileSystem gives access to reading/writing files, copying directories and so on...
      */
    fileSystem: FileSystem,
    /** a custom classloader can set on the environment when specifications must be executed in a specific one, for
      * example when running inside SBT
      */
    customClassLoader: Option[ClassLoader],
    /** this is an indirection allowing Thread.setContextClassLoader with the custom class loader when the platform
      * permits it
      */
    classLoading: ClassLoading,
    /** execution environment for the code *inside* the specification examples */
    executionEnv: ExecutionEnv,
    /** execution environment for the specs2 own reporting */
    specs2ExecutionEnv: ExecutionEnv
):

  lazy val executionContext =
    executionEnv.executionContext

  lazy val executorServices =
    executionEnv.executorServices

  lazy val specs2ExecutionContext =
    specs2ExecutionEnv.executionContext

  lazy val specs2ExecutorServices =
    specs2ExecutionEnv.executorServices

  lazy val timeout =
    arguments.timeout

  lazy val commandLine: CommandLine =
    arguments.commandLine

  lazy val defaultInstances =
    List[AnyRef](arguments.commandLine, executionEnv, executionContext, arguments, this)

  def setTimeout(duration: FiniteDuration): Env =
    copy(arguments = arguments.setTimeout(duration))

  /** @return a list of finalization failures by resource key if any */
  def shutdown: Future[List[Result]] =
    given ExecutionContext = specs2ExecutionContext
    val results: Action[List[(String, Result)]] = resources.toList.traverse { case (key, resource) =>
      resource.finalizer.startExecution(this).executionResult.map(r => (key, r))
    }

    val failures = results
      .map { (rs: List[(String, Result)]) =>
        rs
          .filter(_._2.isIssue)
          .map { case (resourceKey, result) =>
            result.updateMessage(m => s"The resource with key '$resourceKey' could not be finalized: $m")
          }
      }
      .runFuture(specs2ExecutionEnv)

    failures.onComplete { _ =>
      try specs2ExecutionEnv.shutdown()
      finally executionEnv.shutdown()
    }
    failures

  def shutdownResult: Future[Result] =
    given ExecutionContext = specs2ExecutionContext
    shutdown.map(vs => AsResult(vs))

  /** be sure to only call this method on the JVM! */
  def awaitShutdown(): Unit =
    Await.result(shutdown, Duration.Inf)

  /** set new PrinterLogger */
  def setPrinterLogger(logger: PrinterLogger) =
    copy(printerLogger = logger)

  /** set new system logger */
  def setSystemLogger(logger: Logger) =
    copy(systemLogger = logger)

  /** set new arguments */
  def setArguments(args: Arguments) =
    copy(arguments = args)

  /** set a new statistic repository */
  def setStatisticRepository(repository: StatisticsRepository) =
    copy(statisticsRepository = repository)

  /** set a new classloader to be used as the context classloader for each execution */
  def setCustomClassLoader(classLoader: ClassLoader): Env =
    copy(customClassLoader = Some(classLoader))

  /** if a custom classloader is used, give the possibility to set it as the context class loader This is used during
    * the execution of fragments. See Execution.scala
    */
  def setContextClassLoader(): Unit =
    customClassLoader.foreach(classLoading.setContextClassLoader)

// map of resources with a key possibly shared by several specifications
type Resources = mutable.Map[String, ResourceExecution[?]]

enum ResourceType:
  case Local
  case Global

case class ResourceExecution[T](resourceType: ResourceType, resource: Future[T], release: T => Execution):
  def finalizer: Execution = Execution.withEnvFlatten { (env: Env) =>
    given ExecutionContext = env.executionContext
    Execution.futureFlatten(resource.map(release))
  }

object Env:

  def apply(
      arguments: Arguments = EnvDefault.default.arguments,
      resources: Resources = EnvDefault.default.resources,
      systemLogger: Logger = EnvDefault.default.systemLogger,
      printerLogger: PrinterLogger = EnvDefault.default.printerLogger,
      statisticsRepository: StatisticsRepository = EnvDefault.default.statisticsRepository,
      random: scala.util.Random = EnvDefault.default.random,
      fileSystem: FileSystem = EnvDefault.default.fileSystem,
      customClassLoader: Option[ClassLoader] = EnvDefault.default.customClassLoader,
      classLoading: ClassLoading = EnvDefault.default.classLoading
  ): Env =
    Env(
      arguments,
      resources,
      systemLogger,
      printerLogger,
      statisticsRepository,
      random,
      fileSystem,
      customClassLoader,
      classLoading,
      executionEnv = ExecutionEnv.create(arguments, systemLogger),
      specs2ExecutionEnv = ExecutionEnv.createSpecs2(arguments, systemLogger)
    )

  def executeResult[R: AsResult](r: Env => R) =
    lazy val env = Env()
    AsResult(r(env))
