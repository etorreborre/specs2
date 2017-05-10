package org.specs2
package concurrent

import java.util.concurrent._

import org.specs2.control._, eff._
import org.specs2.main.Arguments
import scala.concurrent.ExecutionContext
import ConsoleEffect._

/**
 * Subset of the Env describing execution parameters
 *
 * if withoutIsolation is true then fragments are executed right away because they are
 * already in their own copy of the specification
 */
case class ExecutionEnv(executorService:          Option[ExecutorService],
                        scheduledExecutor: () => ScheduledExecutorService,
                        exContext:         () => ExecutionContext,
                        timeFactor: Int) {
  /** note: shutdown only shuts down the executor services */
  def shutdown(): Unit = {
    try     { executorService.foreach(_.shutdownNow); () }
    finally { scheduledExecutorService.shutdownNow; () }
  }

  lazy val scheduledExecutorService =
    scheduledExecutor()

  val hash = System.identityHashCode(this)

  lazy val executionContext =
    exContext()

  implicit lazy val ses =
    scheduledExecutorService

  implicit lazy val ec =
    executionContext
}

object ExecutionEnv {

  /**
   * create an execution environment
   *  with main executorService for fragments execution (can be used as an execution context)
   *  and a scheduled executor for timed out execution
   */
  def create(arguments: Arguments, systemLogger: Logger, threadFactoryName: String): ExecutionEnv = {
    val executorService = executor(arguments.threadsNb, threadFactoryName)
    ExecutionEnv(
      Some(executorService),
      () => scheduledExecutor(arguments.scheduledThreadsNb, threadFactoryName),
      () => createExecutionContext(executorService, arguments.verbose, systemLogger),
      arguments.execute.timeFactor
    )
  }

  def createExecutionContext(executorService: ExecutorService, verbose: Boolean, systemLogger: Logger) =
    ExecutionContext.fromExecutorService(executorService,
      (t: Throwable) => {runConsoleToPrinter(systemLogger)(logThrowable[Fx1[Console]](t, verbose)); ()})

  /**
   * the number of executors is set from the arguments.threadsNb value which is
   * Runtime.getRuntime.availableProcessors by default
   */
  def executor(threadsNb: Int, name: String): ExecutorService =
    Executors.newFixedThreadPool(threadsNb, NamedThreadFactory("specs2.fixed."+name))

  /**
   * the number of executors is set from the arguments.scheduledThreadsNb value which is
   * 1 by default
   */
  def scheduledExecutor(scheduledThreadsNb: Int, name: String): ScheduledExecutorService =
    Executors.newScheduledThreadPool(scheduledThreadsNb, NamedThreadFactory("specs2.scheduled."+name))


  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: ExecutionContext): ExecutionEnv =
    ExecutionEnv(
      None,
      () => scheduledExecutor(1, "unused"),
      () => ec,
      timeFactor = 1)

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}
