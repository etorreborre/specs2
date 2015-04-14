package org.specs2
package concurrent

import java.util.concurrent._

import org.specs2.control
import org.specs2.control._
import org.specs2.main.Arguments

import scala.concurrent.ExecutionContext
import scalaz.\/
import scalaz.concurrent.Future

/**
 * Subset of the Env describing execution parameters
 *
 * if withoutIsolation is true then fragments are executed right away because they are
 * already in their own copy of the specification
 */
case class ExecutionEnv(executor:          () => ExecutorService,
                        scheduledExecutor: () => ScheduledExecutorService,
                        exContext:         () => ExecutionContext,
                        timeFactor: Int) {

  lazy val timeout = (new Timeout).start

  def withTimeout[T](future: Future[T], timeoutInMillis: Long): Future[SomeTimeout \/ T] =
    timeout.withTimeout(future, timeoutInMillis)

  /** note: shutdown only shuts down the executor services */
  def shutdown(): Unit = {
    try     {
      try executorService.shutdownNow
      finally scheduledExecutorService.shutdownNow
    }
    finally timeout.stop()
  }

  lazy val executorService = executor()
  lazy val scheduledExecutorService = scheduledExecutor()
  lazy val executionContext = exContext()

  implicit lazy val es  = executorService
  implicit lazy val ses = scheduledExecutorService
  implicit lazy val ec  = executionContext
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
      () => executorService,
      () => scheduledExecutor(arguments.scheduledThreadsNb, threadFactoryName),
      () => createExecutionContext(executorService, arguments.verbose, systemLogger),
      arguments.execute.timeFactor
    )
  }

  def createExecutionContext(executorService: ExecutorService, verbose: Boolean, systemLogger: Logger) =
    ExecutionContext.fromExecutorService(executorService,
      (t: Throwable) => control.logThrowable(t, verbose).execute(systemLogger).unsafePerformIO)

  /**
   * the number of executors is set from the arguments.threadsNb value which is
   * Runtime.getRuntime.availableProcessors by default
   */
  def executor(threadsNb: Int, name: String): ExecutorService =
    Executors.newFixedThreadPool(threadsNb, new NamedThreadFactory("specs2.fixed."+name))

  /**
   * the number of executors is set from the arguments.scheduledThreadsNb value which is
   * 1 by default
   */
  def scheduledExecutor(scheduledThreadsNb: Int, name: String): ScheduledExecutorService =
    Executors.newScheduledThreadPool(scheduledThreadsNb, new NamedThreadFactory("specs2.scheduled."+name))


  /** create an ExecutionEnv from an execution context only */
  def fromExecutionContext(ec: ExecutionContext): ExecutionEnv =
    ExecutionEnv(() => executor(1, "unused"), () => scheduledExecutor(1, "unused"), () => ec, timeFactor = 1)

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutionEnv =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

}
