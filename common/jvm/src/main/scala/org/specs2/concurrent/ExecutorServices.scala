package org.specs2.concurrent

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicBoolean

import org.specs2.main._
import org.specs2.control._

case class ExecutorServices(executorServiceEval:          () => ExecutorService,
                            executionContextEval:         () => ExecutionContext,
                            scheduledExecutorServiceEval: () => ScheduledExecutorService,
                            schedulerEval:                () => Scheduler,
                            shutdown:                     () => Unit) {

  private val started = new AtomicBoolean(false)

  implicit lazy val executorService: ExecutorService = {
    started.set(true)
    executorServiceEval()
  }

  implicit lazy val scheduledExecutorService: ScheduledExecutorService = {
    started.set(true)
    scheduledExecutorServiceEval()
  }

  implicit lazy val executionContext: ExecutionContext = {
    started.set(true)
    executionContextEval()
  }

  implicit lazy val scheduler: Scheduler = {
    started.set(true)
    schedulerEval()
  }

  def shutdownNow(): Unit =
    if (started.get) shutdown()

  /** convenience method to shutdown the services when the final future has completed */
  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices = {
    future.onComplete(_ => shutdown())
    this
  }

  def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit =
    scheduler.schedule(timedout, duration)

}

object ExecutorServices {

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
      () => executorService,
      () => executionContext,
      () => scheduledExecutorService,
      () => Schedulers.schedulerFromScheduledExecutorService(scheduledExecutorService),
      () => { try executorService.shutdown finally scheduledExecutorService.shutdown }
    )
  }

  def fromExecutionContext(ec: ExecutionContext): ExecutorServices =
    ExecutorServices(
      () => fixedExecutor(1, "unused"),
      () => ec,
      () => scheduledExecutor(1, "unused"),
      () => Schedulers.default,
      () => ())

  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

  def createExecutionContext(executorService: ExecutorService, verbose: Boolean, systemLogger: Logger): ExecutionContext =
    ExecutionContext.fromExecutorService(executorService,
      (t: Throwable) => { systemLogger.exception(t, verbose).runVoid })

  def fixedExecutor(threadsNb: Int, name: String): ExecutorService =
    Executors.newFixedThreadPool(threadsNb, NamedThreadFactory(name))

  /**
   * the number of executors is set from the arguments.scheduledThreadsNb value which is
   * 1 by default
   */
  def scheduledExecutor(scheduledThreadsNb: Int, name: String): ScheduledExecutorService =
    Executors.newScheduledThreadPool(scheduledThreadsNb, NamedThreadFactory("specs2.scheduled."+name))
}
