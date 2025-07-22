package org.specs2
package matcher

import org.specs2.concurrent.ExecutionEnv
import scala.concurrent.duration.*
import scala.concurrent.*
import execute.*
import concurrent.FutureAwait.{await as futureAwait}
import Result.*

/** This trait is for transforming matchers of values to matchers of Futures
  */
trait FutureMatchers extends ExpectationsCreation:
  /** add an `await` method to any matcher `Matcher[T]` so that it can be transformed into a `Matcher[Future[T]] making
    * this implicit an extension method does not work out of the box`
    */
  extension [T](m: Matcher[T])(using ee: ExecutionEnv)
    def await: Matcher[Future[T]] =
      awaitMatcher(m)(retries = 0, timeout = 1.second)

    def await(retries: Int, timeout: FiniteDuration): Matcher[Future[T]] =
      awaitMatcher(m)(retries, timeout)

    def retryAwait(retries: Int): Matcher[Future[T]] =
      awaitMatcher(m)(retries, timeout = 1.second)

    def awaitFor(timeout: FiniteDuration): Matcher[Future[T]] =
      awaitMatcher(m)(retries = 0, timeout)

  /** when a Future contains a result, it can be awaited to return this result
    */
  implicit def futureToResult[T](f: =>Future[T])(using ee: ExecutionEnv, asResult: AsResult[T]): FutureAsResult[T] =
    FutureAsResult[T](f)

  def await[T](m: Matcher[T])(using ee: ExecutionEnv, nothing: Int = 0): Matcher[Future[T]] =
    awaitMatcher(m)(retries = 0, timeout = 1.second)
  def await[T](m: Matcher[T])(retries: Int, timeout: FiniteDuration)(using ee: ExecutionEnv): Matcher[Future[T]] =
    awaitMatcher(m)(retries, timeout)
  def awaitFor[T](m: Matcher[T])(timeout: FiniteDuration)(using ee: ExecutionEnv): Matcher[Future[T]] =
    awaitMatcher(m)(retries = 0, timeout)
  def retry[T](m: Matcher[T])(retries: Int)(using ee: ExecutionEnv): Matcher[Future[T]] =
    awaitMatcher(m)(retries, timeout = 1.second)

  private[specs2] def awaitMatcher[T](m: Matcher[T])(retries: Int, timeout: FiniteDuration)(using
      ee: ExecutionEnv
  ): Matcher[Future[T]] =
    new Matcher[Future[T]]:
      def apply[S <: Future[T]](a: Expectable[S]) =
        // evaluate the future value as such
        // it the future throws an exception, it will be
        // reported as an error
        val syncFailCapture = a.value
        try
          val futures = Iterator(syncFailCapture) ++ Iterator.continually(a.valueDefinition)
          new FutureAsResult(futures.next.map(v => AsResult(createExpectable(v).applyMatcher(m)))(ee.executionContext))
            .await(retries, timeout)

        catch
          case f: FailureException =>
            throw f
          // if awaiting on the future throws an exception because it was a failed future
          // there try to match again because the matcher can be a `throwA` matcher
          case t: Throwable =>
            createExpectable(throw t).applyMatcher(m)

object FutureMatchers extends FutureMatchers

private[specs2] class FutureAsResult[T](f: =>Future[T])(using ee: ExecutionEnv, asResult: AsResult[T]):
  def await: Result =
    await(retries = 0, timeout = 1.second)

  def retry(retries: Int): Result =
    await(retries, timeout = 1.second)

  def awaitFor(timeout: FiniteDuration): Result =
    await(retries = 0, timeout)

  def await(retries: Int, timeout: FiniteDuration): Result =
    futureAwait(f)(retries, timeout).fold(
      timedout =>
        checkResultFailure(
          Failure(
            s"Timeout after ${timedout.totalDuration + timedout.appliedTimeout} (retries = $retries, timeout = $timeout), timeFactor = ${timedout.timeFactor}"
          )
        ),
      t => asResult.asResult(t)
    )
