package org.specs2.concurrent

import org.specs2.control.eff.Evaluated
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent._
import org.specs2.main._
import org.specs2.control._
import org.specs2.control.eff._
import org.specs2.control.eff.ConsoleEffect._

case class ExecutorServices(executorServiceEval:          Evaluated[ExecutorService],
                            executionContextEval:         Evaluated[ExecutionContext],
                            scheduledExecutorServiceEval: Evaluated[ScheduledExecutorService],
                            schedulerEval:                Evaluated[Scheduler],
                            shutdown:                     Evaluated[Unit]) {

  implicit lazy val executorService: ExecutorService =
    executorServiceEval.value

  implicit lazy val scheduledExecutorService: ScheduledExecutorService =
    scheduledExecutorServiceEval.value

  implicit lazy val executionContext: ExecutionContext =
    executionContextEval.value

  implicit lazy val scheduler: Scheduler =
    schedulerEval.value

  /** convenience method to shutdown the services when the final future has completed */
  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices = {
    future.onComplete(_ => shutdown.value)
    this
  }

  def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit =
    scheduler.schedule(timedout, duration)

}

object ExecutorServices {

  lazy val threadsNb: Int =
    math.max(Runtime.getRuntime.availableProcessors, 4)

  def create(arguments: Arguments, systemLogger: Logger, threadFactoryName: String): ExecutorServices = {
    val executorService = executor(arguments.threadsNb, threadFactoryName)
    val scheduledExecutorService = scheduledExecutor(arguments.scheduledThreadsNb, threadFactoryName)

    ExecutorServices(
      Memoized(executorService),
      Memoized(createExecutionContext(executorService, arguments.verbose, systemLogger)),
      Memoized(scheduledExecutorService),
      Memoized(Schedulers.schedulerFromScheduledExecutorService(scheduledExecutorService)),
      Memoized(executorService.shutdown)
    )
  }

  def fromExecutionContext(ec: ExecutionContext): ExecutorServices =
    ExecutorServices(
      Memoized(executor(1, "unused")),
      Memoized(ec),
      Memoized(scheduledExecutor(1, "unused")),
      Memoized(Schedulers.default),
      Memoized(()))

  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

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
}
