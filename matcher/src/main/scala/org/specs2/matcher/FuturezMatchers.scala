package org.specs2
package matcher

import java.util.concurrent.{ExecutorService, TimeoutException}

import execute.{Failure, Result, AsResult}

import scala.concurrent.duration._
import scala.concurrent.{Future =>_,_}
import scalaz.concurrent.Future

/**
 * This trait is for transforming matchers of values to matchers of scalaz.concurrent.Future
 */
trait FuturezMatchers extends FuturezBaseMatchers { outer =>
  /**
   * add an `attempt` method to any matcher `Matcher[T]` so that it can be transformed into a `Matcher[scalaz.concurrent.Future[T]]`
   */
  implicit class FuturezMatchable[T](m: Matcher[T])(implicit es: ExecutorService) {
    def attempt: Matcher[Future[T]]                                                        = attempt()
    def attempt(retries: Int = 0, timeout: FiniteDuration = 1.seconds): Matcher[Future[T]] = outer.attemptFor(m)(retries, timeout)(es)
  }

  /**
   * when a Future contains a result, it can be attempted to return this result
   */
  implicit class futureAsResult[T](f: Future[T])(implicit es: ExecutorService, asResult: AsResult[T]) extends FuturezAsResult[T](f)
}

private[specs2]
trait FuturezBaseMatchers extends ExpectationsCreation {

  def attempt[T](m: Matcher[T])(implicit es: ExecutorService): Matcher[Future[T]] = attemptFor(m)()
  def attempt[T](m: Matcher[T])(retries: Int = 0, timeout: FiniteDuration = 1.seconds)(implicit es: ExecutorService): Matcher[Future[T]] = attemptFor(m)(retries, timeout)

  private[specs2]
  class FuturezAsResult[T](f: Future[T])(implicit es: ExecutorService, asResult: AsResult[T]) {
    def attempt: Result = attempt()

    def attempt(retries: Int = 0, timeout: FiniteDuration = 1.seconds): Result = {
      def attemptFor(retries: Int, totalDuration: FiniteDuration = 0.seconds): Result = {
        f.map(AsResult(_)).timed(timeout).run.fold({
            case e: TimeoutException =>
              if (retries <= 0) Failure(s"Timeout after ${totalDuration + timeout}")
              else              attemptFor(retries - 1, totalDuration + timeout)

            case other: Throwable    => throw other
          },
          r => r
        )
      }
      attemptFor(retries)
    }
  }

  private[specs2] def attemptFor[T](m: Matcher[T])(retries: Int = 0, timeout: FiniteDuration = 1.seconds)(implicit es: ExecutorService): Matcher[Future[T]] = new Matcher[Future[T]] {
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