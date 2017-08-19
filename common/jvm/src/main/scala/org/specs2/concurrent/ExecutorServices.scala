package org.specs2.concurrent

import org.specs2.control.eff.Evaluated

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicBoolean

import org.specs2.main._
import org.specs2.control._
import org.specs2.control.eff._
import org.specs2.control.eff.ConsoleEffect._

case class ExecutorServices(executorServiceEval:          Evaluated[ExecutorService],
                            executionContextEval:         Evaluated[ExecutionContext],
                            scheduledExecutorServiceEval: Evaluated[ScheduledExecutorService],
                            schedulerEval:                Evaluated[Scheduler],
                            shutdown:                     Evaluated[Unit]) {

  private val started = new AtomicBoolean(false)

  implicit lazy val executorService: ExecutorService = {
    started.set(true)
    executorServiceEval.value
  }

  implicit lazy val scheduledExecutorService: ScheduledExecutorService = {
    started.set(true)
    scheduledExecutorServiceEval.value
  }

  implicit lazy val executionContext: ExecutionContext = {
    started.set(true)
    executionContextEval.value
  }

  implicit lazy val scheduler: Scheduler = {
    started.set(true)
    schedulerEval.value
  }

  def shutdownNow(): Unit =
    if (started.get) shutdown.value

  /** convenience method to shutdown the services when the final future has completed */
  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices = {
    future.onComplete(_ => shutdown.value)
    this
  }

  def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit =
    scheduler.schedule(timedout, duration)

}

object ExecutorServices {

  /** default max threads number for the user execution environment */
  lazy val threadsNb: Int =
    math.max(Runtime.getRuntime.availableProcessors, 4)

  /** default threads number for the specs2 execution environment */
  lazy val specs2ThreadsNb: Int =
    math.max(Runtime.getRuntime.availableProcessors, 4)

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutorServices =
    createExecutorServices(arguments, systemLogger, tag, isSpecs2 = false)

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutorServices =
    createExecutorServices(arguments, systemLogger, tag, isSpecs2 = true)

  private def createExecutorServices(arguments: Arguments, systemLogger: Logger, tag: Option[String], isSpecs2: Boolean): ExecutorServices = {
    val threadFactoryName: String =
      if (isSpecs2) "specs2"+tag.map("-"+_).getOrElse("")
      else          "specs2.user"+tag.map("-"+_).getOrElse("")

    lazy val executorService =
      if (isSpecs2) fixedExecutor(arguments.specs2ThreadsNb, threadFactoryName)
      else          fixedExecutor(arguments.threadsNb, threadFactoryName)

    lazy val scheduledExecutorService =
      scheduledExecutor(arguments.scheduledThreadsNb, threadFactoryName)

    lazy val executionContext =
      createExecutionContext(executorService, arguments.verbose, systemLogger)

    ExecutorServices(
      Memoized(executorService),
      Memoized(executionContext),
      Memoized(scheduledExecutorService),
      Memoized(Schedulers.schedulerFromScheduledExecutorService(scheduledExecutorService)),
      Memoized { try executorService.shutdown finally scheduledExecutorService.shutdown }
    )
  }

  def fromExecutionContext(ec: ExecutionContext): ExecutorServices =
    ExecutorServices(
      Memoized(fixedExecutor(1, "unused")),
      Memoized(ec),
      Memoized(scheduledExecutor(1, "unused")),
      Memoized(Schedulers.default),
      Memoized(()))

  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

  def createExecutionContext(executorService: ExecutorService, verbose: Boolean, systemLogger: Logger) =
    ExecutionContext.fromExecutorService(executorService,
      (t: Throwable) => {runConsoleToPrinter(systemLogger)(logThrowable[Fx1[Console]](t, verbose)); ()})

  def fixedExecutor(threadsNb: Int, name: String): ExecutorService =
    Executors.newFixedThreadPool(threadsNb, NamedThreadFactory(name))

  /**
   * the number of executors is set from the arguments.scheduledThreadsNb value which is
   * 1 by default
   */
  def scheduledExecutor(scheduledThreadsNb: Int, name: String): ScheduledExecutorService =
    Executors.newScheduledThreadPool(scheduledThreadsNb, NamedThreadFactory("specs2.scheduled."+name))
}
