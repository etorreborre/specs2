package org.specs2
package matcher

import concurrent.duration._
import scala.concurrent.{ExecutionContext, Await, Future}
import java.util.concurrent.TimeoutException
import execute.{AsResult, Failure, Result}

/**
 * This trait is for transforming matchers of values to matchers of Futures
 */
trait FutureMatchers extends Expectations with ConcurrentExecutionContext {

  /**
   * add an `await` method to any matcher `Matcher[T]` so that it can be transformed into a `Matcher[Future[T]]`
   */
  implicit class FutureMatchable[T](m: Matcher[T]) {
    def await: Matcher[Future[T]]                                                        = await()
    def await(retries: Int = 0, timeout: FiniteDuration = 1.seconds): Matcher[Future[T]] = awaitFor(m)(retries, timeout)
  }

  /**
   * when a Future contains a result, it can be awaited to return this result
   */
  implicit class futureAsResult[T : AsResult](f: Future[T]) {
    def await: Result = await()
    def await(retries: Int = 0, timeout: FiniteDuration = 1.seconds): Result = {
      def awaitFor(retries: Int, totalDuration: FiniteDuration = 0.seconds): Result = {
        try Await.result(f.map(value => AsResult(value)), timeout)
        catch {
          case e: TimeoutException =>
            if (retries <= 0) Failure(s"Timeout after ${totalDuration + timeout}")
            else awaitFor(retries - 1, totalDuration + timeout)
          case other: Throwable    => throw other
        }
      }
      awaitFor(retries)
    }
  }

  def await[T](m: Matcher[T]): Matcher[Future[T]] = awaitFor(m)()
  def await[T](m: Matcher[T])(retries: Int = 0, timeout: FiniteDuration = 1.seconds): Matcher[Future[T]] = awaitFor(m)(retries, timeout)

  private def awaitFor[T](m: Matcher[T])(retries: Int = 0, timeout: FiniteDuration = 1.seconds): Matcher[Future[T]] = new Matcher[Future[T]] {
    def apply[S <: Future[T]](a: Expectable[S]) = {
      try {
        val r = a.value.map(v => createExpectable(v).applyMatcher(m).toResult).await(retries, timeout)
        result(r.isSuccess, r.message, r.message, a)
      } catch {
        // if awaiting on the future throws an exception because it was a failed future
        // there try to match again because the matcher can be a `throwA` matcher
        case t: Throwable =>
          val r = createExpectable(throw t).applyMatcher(m).toResult
          result(r.isSuccess, r.message, r.message, a)
      }
    }
  }
}

object FutureMatchers extends FutureMatchers

/**
 * Specification of the execution context to be used for executing futures
 * This can be overridden to pass in your own execution context
 */
trait ConcurrentExecutionContext {
  implicit def concurrentExecutionContext: ExecutionContext = concurrent.ExecutionContext.Implicits.global
}

/**
 * stack this trait to remove the implicit execution context used to evaluate features
 */
trait NoConcurrentExecutionContext extends ConcurrentExecutionContext {
  override def concurrentExecutionContext: ExecutionContext = super.concurrentExecutionContext
}
