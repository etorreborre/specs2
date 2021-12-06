package org.specs2
package matcher

import org.specs2.concurrent.ExecutionEnv
import scala.concurrent.duration.*
import scala.concurrent.*
import execute.*
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
        sys.error(
          "awaiting on futures is not allowed in ScalaJS. You can not use features from the FutureMatchers trait"
        )

object FutureMatchers extends FutureMatchers

private[specs2] class FutureAsResult[T](f: =>Future[T])(using ee: ExecutionEnv, asResult: AsResult[T]):
  def await: Result =
    await(retries = 0, timeout = 1.second)

  def retry(retries: Int): Result =
    await(retries, timeout = 1.second)

  def awaitFor(timeout: FiniteDuration): Result =
    await(retries = 0, timeout)

  def await(retries: Int, timeout: FiniteDuration): Result =
    sys.error("awaiting on futures is not allowed in ScalaJS. You can not use features from the FutureMatchers trait")
