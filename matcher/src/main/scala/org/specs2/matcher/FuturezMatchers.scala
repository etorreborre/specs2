package org.specs2
package matcher

import java.util.concurrent._
import execute._
import scala.concurrent.duration._
import scala.concurrent.{Future =>_,_}
import scalaz.concurrent.{Strategy, Future}

/**
 * This trait is for transforming matchers of values to matchers of scalaz.concurrent.Future
 */
trait FuturezMatchers extends FuturezBaseMatchers { outer =>
  /**
   * add an `attempt` method to any matcher `Matcher[T]` so that it can be transformed into a `Matcher[scalaz.concurrent.Future[T]]`
   */
  implicit class FuturezMatchable[T](m: Matcher[T])(implicit es: ExecutorService) {
    def attempt: Matcher[Future[T]]                                        = attemptMatcher(m)(retries = 0, timeout = 1.second)(es)
    def attempt(retries: Int, timeout: FiniteDuration): Matcher[Future[T]] = attemptMatcher(m)(retries, timeout)(es)
    def retryAttempt(retries: Int): Matcher[Future[T]]                     = attemptMatcher(m)(retries, timeout = 1.second)(es)
    def attemptFor(timeout: FiniteDuration): Matcher[Future[T]]            = attemptMatcher(m)(retries = 0, timeout)(es)
  }

  /**
   * when a Future contains a result, it can be attempted to return this result
   */
  implicit class futureAsResult[T](f: Future[T])(implicit es: ExecutorService, asResult: AsResult[T]) extends FuturezAsResult[T](f)
}

private[specs2]
trait FuturezBaseMatchers extends ExpectationsCreation with ExecutionTimeFactor {

  def attempt[T](m: Matcher[T])(implicit es: ExecutorService): Matcher[Future[T]] = attemptMatcher(m)(retries = 0, timeout = 1.second)
  def attempt[T](m: Matcher[T])(retries: Int, timeout: FiniteDuration)(implicit es: ExecutorService): Matcher[Future[T]] = attemptMatcher(m)(retries, timeout)
  def attempt[T](m: Matcher[T])(retries: Int)(implicit es: ExecutorService): Matcher[Future[T]] = attemptMatcher(m)(retries, timeout = 1.second)
  def attempt[T](m: Matcher[T])(timeout: FiniteDuration)(implicit es: ExecutorService): Matcher[Future[T]] = attemptMatcher(m)(0, timeout)

  private[specs2]
  class FuturezAsResult[T](f: Future[T])(implicit es: ExecutorService, asResult: AsResult[T]) {
    def attempt: Result =
      attempt(retries = 0, timeout = 1.second)

    def attemptFor(timeout: FiniteDuration): Result =
      attempt(retries = 0, timeout)

    def attempt(retries: Int, timeout: FiniteDuration): Result = {
      val tf = timeFactor(es)
      val appliedTimeout = timeout * tf

      def attemptFuture(remainingRetries: Int, totalDuration: FiniteDuration): Result = {
        implicit val ses = ee.scheduledExecutorService
        f.map(AsResult(_)).timed(appliedTimeout.toMillis).run.fold({
            case e: TimeoutException =>
              if (remainingRetries <= 0) Failure(s"Timeout after ${totalDuration + appliedTimeout} (retries = $retries, timeout = $timeout)")
              else                       attemptFuture(remainingRetries - 1, totalDuration + appliedTimeout)

            case other: Throwable => throw other
          },
          r => r
        )
      }
      attemptFuture(retries, 0.second)
    }
  }

  private[specs2] def attemptMatcher[T](m: Matcher[T])(retries: Int, timeout: FiniteDuration)(implicit ee: ExecutionEnv): Matcher[Future[T]] = new Matcher[Future[T]] {
    def apply[S <: Future[T]](a: Expectable[S]) = {
      try {
        val r = new FuturezAsResult(a.value.map(v => createExpectable(v).applyMatcher(m).toResult)).attempt(retries, timeout)
        result(r.isSuccess, r.message, r.message, a)
      } catch {
        // if attempting on the future throws an exception because it was a failed future
        // there try to match again because the matcher can be a `throwA` matcher
        case t: Throwable =>
          val r = createExpectable(throw t).applyMatcher(m).toResult
          result(r.isSuccess, r.message, r.message, a)
      }
    }
  }
}

object FuturezMatchers extends FuturezMatchers