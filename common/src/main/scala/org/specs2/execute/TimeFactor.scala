package org.specs2
package execute


import java.util
import java.util.concurrent.{Callable, TimeUnit, ExecutorService}

import scala.concurrent.ExecutionContext

/**
 * trait for setting a time factor on an ExecutionContext or an ExecutorService
 */
trait TimeFactor {
  def timeFactor: Int
}

/**
 * Get the time factor from an ExecutionContext or an ExecutorService
 */
trait ExecutionTimeFactor {
  def timeFactor(ec: ExecutionContext): Int =
    ec match {
      case context: ExecutionContext with TimeFactor => context.timeFactor
      case _ => 1
    }

  def timeFactor(es: ExecutorService): Int =
    es match {
      case service: ExecutorService with TimeFactor => service.timeFactor
      case _ => 1
    }
}

/**
 * Set a time factor on an ExecutionContext or an ExecutorService by decorating it with the TimeFactor trait
 */
object ExecutionTimeFactor {
  def decorateExecutionContext(ec: ExecutionContext, factor: Int): ExecutionContext = new ExecutionContext with TimeFactor {
    def timeFactor: Int =
      factor

    def execute(runnable: Runnable): Unit =
      ec.execute(runnable)

    def reportFailure(@deprecatedName('t) cause: Throwable): Unit =
      ec.reportFailure(cause)

    override def prepare(): ExecutionContext =
      ec.prepare()
  }

  def decorateExecutorService(es: ExecutorService, factor: Int): ExecutorService = new ExecutorService with TimeFactor {
    def timeFactor: Int =
      factor

    def execute (command: Runnable): Unit =
      es.execute(command)

    def shutdown: Unit =
      es.shutdown

    def shutdownNow(): java.util.List[Runnable] =
      es.shutdownNow()

    def isShutdown(): Boolean =
      es.isShutdown()

    def isTerminated(): Boolean =
      es.isTerminated()

    def awaitTermination(timeout: Long, unit: TimeUnit) =
      es.awaitTermination(timeout: Long, unit)

    def submit[T](task: Callable[T]): java.util.concurrent.Future[T] =
      es.submit(task)

    def submit[T](task: Runnable, result: T): java.util.concurrent.Future[T] =
      es.submit(task, result)

    def submit(task: Runnable): java.util.concurrent.Future[_] =
      es.submit(task)

    def invokeAll[T](tasks: util.Collection[_ <: Callable[T]]): java.util.List[java.util.concurrent.Future[T]] =
      es.invokeAll(tasks)


    def invokeAll[T](tasks: util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): java.util.List[java.util.concurrent.Future[T]] =
      es.invokeAll(tasks, timeout, unit)

    def invokeAny[T](tasks: util.Collection[_ <: Callable[T]]): T =
      es.invokeAny(tasks)

    def invokeAny[T](tasks: util.Collection[_ <: Callable[T]], timeout: Long, unit: TimeUnit): T =
      es.invokeAny(tasks, timeout, unit)
  }

}
