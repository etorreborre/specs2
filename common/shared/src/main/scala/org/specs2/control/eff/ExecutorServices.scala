package org.specs2.control.eff

import java.util.Collections
import java.util.concurrent._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

case class ExecutorServices(executorServiceEval:   Evaluated[ExecutorService],
                            schedulerEval:         Evaluated[Scheduler],
                            executionContextEval:  Evaluated[ExecutionContext]) {

  /** note: shutdown only shuts down the executor services */
  def shutdown: Evaluated[Unit] = Memoized {
    // careful: calling executorService.shutdown or scheduledExecutorService will deadlock!
    try     executorServiceEval.value.shutdown
    finally schedulerEval.value.shutdown
  }

  implicit lazy val executorService: ExecutorService =
    executorServiceEval.value

  implicit lazy val executionContext: ExecutionContext =
    executionContextEval.value

  implicit lazy val scheduler: Scheduler =
    schedulerEval.value

  /** convenience method to shutdown the services when the final future has completed */
  def shutdownOnComplete[A](future: scala.concurrent.Future[A]): ExecutorServices = {
    future.onComplete(_ => shutdown.value)
    this
  }

}

object ExecutorServices {

  lazy val threadsNb = Runtime.getRuntime.availableProcessors

  def create(implicit es: ExecutorService, s: ScheduledExecutorService): ExecutorServices =
    fromExecutorServices(es, s)

  def fromExecutorServices(es: =>ExecutorService, s: =>ScheduledExecutorService): ExecutorServices =
    ExecutorServices(
      Memoized(es),
      Memoized(schedulerFromScheduledExecutorService(s)),
      Memoized(createExecutionContext(es))
    )

  def fromExecutorService(es: =>ExecutorService): ExecutorServices =
    fromExecutorServices(es, scheduledExecutor(threadsNb))

  def createExecutionContext(executorService: ExecutorService, logger: String => Unit = println): ExecutionContext =
    ExecutionContext.fromExecutorService(executorService, (t: Throwable) => logger(t.getStackTrace.mkString("\n")))

  def executor(threadsNb: Int): ExecutorService =
    Executors.newFixedThreadPool(threadsNb)

  def scheduledExecutor(scheduledThreadsNb: Int): ScheduledExecutorService =
    Executors.newScheduledThreadPool(scheduledThreadsNb)


  /**
   * create an ExecutionEnv from an execution context only
   *
   * WARNING!!! This method create a brand new scheduledExecutorService which will be used if
   * you use the ExecutorServices to timeout an Async effect
   */
  def fromExecutionContext(ec: =>ExecutionContext): ExecutorServices =
    ExecutorServices(
      Memoized(executorFromExecutionContext(ec)),
      Memoized(schedulerFromScheduledExecutorService(scheduledExecutor(threadsNb))),
      Memoized(ec))

  /** taken from https://gist.github.com/viktorklang/5245161 */
  def executorFromExecutionContext(ec: =>ExecutionContext): ExecutorService =
    ec match {
      case null => throw null
      case eces: ExecutionContextExecutorService => eces
      case other => new AbstractExecutorService with ExecutionContextExecutorService {
        override def prepare(): ExecutionContext = other
        override def isShutdown = false
        override def isTerminated = false
        override def shutdown() = ()
        override def shutdownNow() = Collections.emptyList[Runnable]
        override def execute(runnable: Runnable): Unit = other execute runnable
        override def reportFailure(t: Throwable): Unit = other reportFailure t
        override def awaitTermination(length: Long, unit: TimeUnit): Boolean = false
      }
    }

  /** create an ExecutionEnv from Scala global execution context */
  def fromGlobalExecutionContext: ExecutorServices =
    fromExecutionContext(scala.concurrent.ExecutionContext.global)

  /** create a Scheduler from Scala global execution context */
  def schedulerFromGlobalExecutionContext: Scheduler =
    schedulerFromScheduledExecutorService(scheduledExecutor(threadsNb))

  def schedulerFromScheduledExecutorService(s: ScheduledExecutorService): Scheduler =
    new Scheduler {
      def schedule(timedout: =>Unit, duration: FiniteDuration): () => Unit = {
        val scheduled = s.schedule(new Runnable { def run(): Unit = timedout }, duration.toNanos, TimeUnit.NANOSECONDS)
        () => { scheduled.cancel(false); () }
      }

      def shutdown(): Unit =
        s.shutdown

      override def toString = "Scheduler"
    }

}


