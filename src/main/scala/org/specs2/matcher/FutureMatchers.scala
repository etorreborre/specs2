package org.specs2
package matcher

import concurrent.duration._
import concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import java.util.concurrent.TimeoutException

/**
 * This trait is for transforming matchers of values to matchers of Futures
 */
trait FutureMatchers extends Expectations {

  /**
   * add an `await` method to any matcher `Matcher[T]` so that it can be transformed into a `Matcher[Future[T]]`
   */
  implicit class FutureMatchable[T](m: Matcher[T]) {
    def await: Matcher[Future[T]]                                                        = await()
    def await(retries: Int = 0, timeout: FiniteDuration = 1.seconds): Matcher[Future[T]] = awaitFor(m)(retries, timeout)

  }

  def await[T](m: Matcher[T])(retries: Int = 0, timeout: FiniteDuration = 1.seconds): Matcher[Future[T]] = awaitFor(m)(retries, timeout)

  private def awaitFor[T](m: Matcher[T])(retries: Int = 0, timeout: FiniteDuration = 1.seconds, totalDuration: FiniteDuration = 0.seconds): Matcher[Future[T]] = new Matcher[Future[T]] {
    def apply[S <: Future[T]](a: Expectable[S]) = {
      try {
        val r = m(createExpectable(Await.result(a.value, atMost = timeout)))
        result(r.isSuccess, r.message, r.message, a)
      }
      catch {
        case e: TimeoutException => if (retries <= 0) result(false, "Timeout ok", "Timeout after "+totalDuration, a) else awaitFor(m)(retries - 1, timeout, totalDuration + timeout)(a)
        case other: Throwable    => throw other
      }
    }
  }

}

object FutureMatchers extends FutureMatchers
