package org.specs2
package specification
package core

import main.{Arguments, CommandLine}
import execute._
import org.specs2.concurrent.ExecutionEnv
import reporter.LineLogger
import io._
import control._
import process.{Executor, Selector, StatisticsRepository}
import EnvDefault._
import reflect._

import scala.concurrent.duration.FiniteDuration

/**
 * Whole creation / execution / reporting environment for a specification
 *
 * Part of it is driven by the command-line, part of it is specs2 implementation
 * Yet it can be used to drive the creation or execution of examples
 *
 * Note: creating an Env instance is a delicate activity because the Env hold a thread pool that needs
 *       to be shutdown at the end of the execution
 */
case class Env(
          arguments:           Arguments                         = default.arguments,
          systemLogger:        Logger                            = default.systemLogger,
          selectorInstance:    Arguments => Selector             = default.selectorInstance,
          executorInstance:    Arguments => Executor             = default.executorInstance,
          lineLogger:          LineLogger                        = default.lineLogger,
          statsRepository:     Arguments => StatisticsRepository = default.statsRepository,
          random:              scala.util.Random                 = default.random,
          fileSystem:          FileSystem                        = default.fileSystem,
          executionParameters: ExecutionParameters               = default.executionParameters,
          customClassLoader:   Option[ClassLoader]               = default.customClassLoader,
          classLoading:        ClassLoading                      = default.classLoading) {

  lazy val executionEnv: ExecutionEnv =
    ExecutionEnv.create(arguments, systemLogger, "env"+hash)

  lazy val specs2ExecutionEnv: ExecutionEnv =
    ExecutionEnv.create(arguments, systemLogger, "specs2-env"+hash)

  lazy val statisticsRepository: StatisticsRepository =
    statsRepository(arguments)

  lazy val selector = selectorInstance(arguments)

  lazy val executor = executorInstance(arguments)

  val hash = System.identityHashCode(this)

  def executionContext =
    executionEnv.executionContext

  def executorServices =
    executionEnv.executorServices

  def specs2ExecutionContext =
    specs2ExecutionEnv.executionContext

  lazy val timeout =
    executionParameters.timeout

  lazy val commandLine: CommandLine =
    arguments.commandLine

  def defaultInstances =
    List(arguments.commandLine, executionEnv, executionContext, arguments, this)

  def setTimeout(duration: FiniteDuration): Env =
    copy(executionParameters = executionParameters.setTimeout(duration))

  def shutdown(): Unit = {
    try {
      //println("shutdown specs2 execution env for "+System.identityHashCode(this))
      specs2ExecutionEnv.shutdown
    }
    finally {
      //println("shutdown execution env for "+System.identityHashCode(this))
      executionEnv.shutdown
    }
  }

  /** set new LineLogger */
  def setLineLogger(logger: LineLogger) =
    copy(lineLogger = logger)

  /** set new system logger */
  def setSystemLogger(logger: String => Unit) =
    copy(systemLogger = logger)

  /** set new arguments */
  def setArguments(args: Arguments) =
    copy(arguments = args)

  /** @return an isolated env */
  def setWithIsolation =
    copy(executionParameters = executionParameters.setWithIsolation)

  /** @return a non isolated env */
  def setWithoutIsolation =
    copy(executionParameters = executionParameters.setWithoutIsolation)

  /** set a new statistic repository */
  def setStatisticRepository(repository: StatisticsRepository) =
    copy(statsRepository = (args: Arguments) => repository)

  /** set a new classloader to be used as the context classloader for each execution */
  def setCustomClassLoader(classLoader: ClassLoader): Env =
    copy(customClassLoader = Some(classLoader))

  def setContextClassLoader(): Unit =
    customClassLoader.foreach(classLoading.setContextClassLoader)
}

object Env {

  def executeResult[R : AsResult](r: Env => R) = {
    lazy val env = Env()
    AsResult(r(env))
  }

}

case class ExecutionParameters(timeout:       Option[FiniteDuration] = None,
                               withIsolation: Boolean = true) {

  def withoutIsolation: Boolean =
    !withIsolation

  /**
   * fragments must be created as "isolated"
   */
  def setWithIsolation: ExecutionParameters =
    copy(withIsolation = true)

  /**
   * fragments must be created as non "isolated"
   */
  def setWithoutIsolation: ExecutionParameters =
    copy(withIsolation = false)

  def setTimeout(duration: FiniteDuration): ExecutionParameters =
    copy(timeout = Some(duration))
}
