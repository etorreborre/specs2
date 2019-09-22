package org.specs2
package execute

import scala.concurrent.duration._

/**
 * This trait adds the possibility to retry a given value, convertible to a result, until it succeeds.
 *
 * This was adapted from a contribution by @robey (http://robey.lag.net)
 */
trait EventuallyResults {

  /**
   * @param sleep the function applied on the retry number (first is 1)
   * @return a matcher that will retry the nested matcher a given number of times
   *
   * {{{
   * eventually(retries = 2, sleep = _ * 100.milliseconds) {
   *   aResult
   * }
   * }}}
   */
  def eventually[T : AsResult](retries: Int, sleep: Int => Duration)(result: =>T): T = {
    val max = retries - 1

    @annotation.tailrec def retry(retried: Int): T = {
      if (retried == max) {
        result
      } else {
        lazy val t = result
        val check = ResultExecution.execute(t)(AsResult(_))

        if (check.isSuccess) {
          t
        } else {
          val pause = sleep(retried).toMillis
          Thread.sleep(pause)
          retry(retried + 1)
        }
      }
    }

    if (retries <= 1) {
      result
    } else {
      retry(0)
    }
  }

  /**
   * @return a matcher that will retry the nested matcher a given number of times
   */
  def eventually[T : AsResult](retries: Int, sleep: Duration)(result: =>T): T =
    eventually[T](retries, (_: Int) => sleep)(result)

  /** @return a result that is retried at least 40 times until it's ok */
  def eventually[T : AsResult](result: =>T): T =
    eventually(40, (_: Int) => 100.millis)(result)
}

object EventuallyResults extends EventuallyResults
