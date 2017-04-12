package org.specs2.concurrent

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import scala.concurrent.duration.FiniteDuration

object Schedulers {

  lazy val threadsNb = Runtime.getRuntime.availableProcessors

  def scheduledExecutor(scheduledThreadsNb: Int): ScheduledExecutorService =
    Executors.newScheduledThreadPool(scheduledThreadsNb)

  def default: Scheduler =
    schedulerFromScheduledExecutorService(scheduledExecutor(threadsNb))

  /** create a Scheduler from the Scala global execution context */
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
