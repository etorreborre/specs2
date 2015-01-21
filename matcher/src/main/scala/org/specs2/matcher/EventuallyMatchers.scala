package org.specs2
package matcher

import time.Duration
import time.TimeConversions._
import org.specs2.execute.{ResultExecution, EventuallyResults}

/**
 * This trait adds the possibility to retry a given matcher until it succeeds.
 *
 * This was contributed by @robey (http://robey.lag.net)
 */
trait EventuallyMatchers extends EventuallyResults {
  
  /**
   * @return a matcher that will retry the nested matcher a given number of times
   */
  def eventually[T](nested: =>Matcher[T], retries: Int, sleep: Duration): Matcher[T] = new Matcher[T] {
    def apply[S <: T](a: Expectable[S]) = retry(retries, sleep, a)

    def retry[S <: T](retries: Int, sleep: Duration, a: Expectable[S]): MatchResult[S] = {
      lazy val matchResult = nested(a.evaluateOnce)
      val result = ResultExecution.execute(matchResult.toResult)
      if (result.isSuccess || retries <= 1)
        matchResult
      else {
        Thread.sleep(sleep.inMillis)
        retry(retries - 1, sleep, a)
      }
    }
  }

  /** @return a matcher that will retry the nested matcher 40 times  */
  def eventually[T](nested: =>Matcher[T]): Matcher[T] = eventually(nested, 40, 100.milliseconds)
}
object EventuallyMatchers extends EventuallyMatchers 