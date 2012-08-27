package org.specs2
package execute

import time.Duration
import time.TimeConversions._

/**
 * This trait adds the possibility to retry a given value, convertible to a result, until it succeeds.
 *
 * This was adapted from a contribution by @robey (http://robey.lag.net)
 */
trait EventuallyResults {

  /**
   * @return a matcher that will retry the nested matcher a given number of times
   */
  def eventually[T : AsResult](retries: Int, sleep: Duration)(result: =>T): T = {
    def retry(retries: Int, sleep: Duration, r: =>T): T = {
      lazy val t = r
      val result = ResultExecution.execute(t)(AsResult(_))
      if (result.isSuccess || retries == 1)
        t
      else {
        Thread.sleep(sleep.inMillis)
        retry(retries - 1, sleep, r)
      }
    }
    retry(retries, sleep, result)
  }

  /** @return a result that is retried at least 40 times until it's ok */
  def eventually[T : AsResult](result: =>T): T = eventually(40, 100.milliseconds)(result)
}
object EventuallyResults extends EventuallyResults