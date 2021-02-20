package org.specs2
package matcher

import execute.*
import execute.ResultExecution.execute
import scala.concurrent.duration.*

/**
 * This trait adds the possibility to retry a given matcher until it succeeds.
 *
 * This was contributed by @robey (http://robey.lag.net)
 */
trait EventuallyMatchers extends EventuallyResults:
  /**
   * @param sleep the function applied on the retry number (first is 1)
   * @return a matcher that will retry the nested matcher a given number of times
   */
  def eventually[T](nested: =>Matcher[T], retries: Int, sleep: Int => Duration): Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](a: Expectable[S]) = retry(0, a)

      @annotation.tailrec
      def retry[S <: T](retried: Int, a: Expectable[S]): Result =
        lazy val matchResult = nested(a.evaluateOnce)
        val result = matchResult.execute

        if result.isSuccess || retries <= 1 || retried == retries then
          result
        else
          val pause = sleep(retried).toMillis
          Thread.sleep(pause)
          retry(retried + 1, a)

  /**
   * @return a matcher that will retry the nested matcher a given number of times
   */
  def eventually[T](nested: =>Matcher[T], retries: Int, sleep: Duration): Matcher[T] =
    eventually[T](nested, retries, (_: Int) => sleep)

  /** @return a matcher that will retry the nested matcher 40 times  */
  def eventually[T](nested: =>Matcher[T]): Matcher[T] =
    eventually(nested, 40, (_: Int) => 100.millis)

object EventuallyMatchers extends EventuallyMatchers
