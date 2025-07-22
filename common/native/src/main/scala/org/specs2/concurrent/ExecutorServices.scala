package org.specs2.concurrent

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicBoolean

import org.specs2.main.*
import org.specs2.control.*
import scala.compiletime.ops.boolean

case class ExecutorServices(
    executorServiceEval: () => ExecutorService,
    executionContextEval: () => ExecutionContext,
    scheduledExecutorServiceEval: () => ScheduledExecutorService,
    schedulerEval: () => Scheduler,
    shutdown: () => Unit
):

  private val started = new AtomicBoolean(false)

  given executorService: ExecutorService =
    started.set(true)
    executorServiceEval()

  given scheduledExecutorService: ScheduledExecutorService =
    started.set(true)
    scheduledExecutorServiceEval()

  given executionContext: ExecutionContext =
    started.set(true)
    executionContextEval()

  given scheduler: Scheduler =
    started.set(true)
    schedulerEval()

  def shutdownNow(): Unit =
    if started.get then shutdown()

  /** convenience method to shutdown the services when the final future has completed */
  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices =
    future.onComplete(_ => shutdown())
    this

  def schedule(action: =>Unit, duration: FiniteDuration): () => Unit =
    scheduler.schedule(action, duration)

object ExecutorServices:

  def create(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutorServices =
    createExecutorServices(arguments, systemLogger, tag, isSpecs2 = false)

  def createSpecs2(arguments: Arguments, systemLogger: Logger, tag: Option[String] = None): ExecutorServices =
    createExecutorServices(arguments, systemLogger, tag, isSpecs2 = true)

  private def createExecutorServices(
      arguments: Arguments,
      systemLogger: Logger,
      tag: Option[String],
      isSpecs2: Boolean
  ): ExecutorServices =
    val threadFactoryName: String =
      if isSpecs2 then "specs2" + tag.map("-" + _).getOrElse("")
      else "specs2.user" + tag.map("-" + _).getOrElse("")

    lazy val executorService =
      if isSpecs2 then fixedExecutor(threadFactoryName, arguments.specs2ThreadsNb, arguments.discardRejectedFutures)
      else fixedExecutor(threadFactoryName, arguments.threadsNb, arguments.discardRejectedFutures)

    lazy val scheduledExecutorService =
      scheduledExecutor(arguments.scheduledThreadsNb, threadFactoryName)

    lazy val executionContext =
      createExecutionContext(executorService, arguments.verbose, systemLogger)

    ExecutorServices(
      () => executorService,
      () => executionContext,
      () => scheduledExecutorService,
      () => Schedulers.schedulerFromScheduledExecutorService(scheduledExecutorService),
      () => {
        try executorService.shutdown
        finally scheduledExecutorService.shutdown
      }
    )

  def fromExecutionContext(ec: ExecutionContext): ExecutorServices =
    ExecutorServices(
      () => fixedExecutor("unused", 1, true),
      () => ec,
      () => scheduledExecutor(1, "unused"),
      () => Schedulers.default,
      () => ()
    )

  def createExecutionContext(
      executorService: ExecutorService,
      verbose: Boolean,
      systemLogger: Logger
  ): ExecutionContext =
    ExecutionContext.fromExecutor(
      executorService,
      (t: Throwable) => { systemLogger.exception(t, verbose).runVoid }
    )

  def fixedExecutor(name: String, threadsNb: Int, discardRejected: Boolean): ExecutorService = {
    val threadPool = new ThreadPoolExecutor(
      threadsNb,
      threadsNb,
      0L,
      TimeUnit.MILLISECONDS,
      new LinkedBlockingQueue[Runnable](),
      NamedThreadFactory(name)
    );

    if discardRejected then threadPool.setRejectedExecutionHandler(new ThreadPoolExecutor.DiscardPolicy)
    threadPool
  }

  /** the number of executors is set from the arguments.scheduledThreadsNb value which is 1 by default
    */
  def scheduledExecutor(scheduledThreadsNb: Int, name: String): ScheduledExecutorService =
    Executors.newScheduledThreadPool(scheduledThreadsNb, NamedThreadFactory("specs2.scheduled." + name))
