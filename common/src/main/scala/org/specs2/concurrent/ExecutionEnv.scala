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
case class ExecutionEnv(executorService: ExecutorService,
                        scheduledExecutorService: ScheduledExecutorService,
                        executionContext: ExecutionContext,
                        timeFactor: Int) {

  lazy val timeout = (new Timeout).start

  def withTimeout[T](future: Future[T], timeoutInMillis: Long): Future[SomeTimeout \/ T] =
    timeout.withTimeout(future, timeoutInMillis)

  def shutdown(): Unit = {
    try     executorService.shutdownNow
    finally timeout.stop()
  }

  implicit val es  = executorService
  implicit val ses = scheduledExecutorService
  implicit val ec  = executionContext
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
      executorService,
      scheduledExecutor(arguments.scheduledThreadsNb, threadFactoryName),
      createExecutionContext(executorService, arguments.verbose, systemLogger),
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
}
