package org.specs2
package matcher

import time.Duration
import time.TimeConversions._

trait EventuallyMatchers {
  
  /**
   * @return a matcher that will retry the nested matcher a given number of times
   */
  def eventually[T](retries: Int, sleep: Duration)(nested: =>Matcher[T]): Matcher[T] = new Matcher[T] {
    def apply[S <: T](a: =>Expectable[S]) = retry(retries, sleep, a)

    def retry[S <: T](retries: Int, sleep: Duration, a: =>Expectable[S]): MatchResult[S] = {
      val result = nested(a.evaluate)
      if (result.isSuccess || retries == 1)
        result
      else {
        Thread.sleep(sleep.inMillis)
        retry(retries - 1, sleep, a)
      }
    }
  }

  /** @return a matcher that will retry the nested matcher a given 40 times  */
  def eventually[T](nested: =>Matcher[T]): Matcher[T] = eventually(40, 100.milliseconds)(nested)
}
object EventuallyMatchers extends EventuallyMatchers 