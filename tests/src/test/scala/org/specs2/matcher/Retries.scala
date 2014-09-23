package org.specs2
package matcher

import execute.{EventuallyResults, AsResult}
import specification.AroundEach
import scala.concurrent.duration._

trait Retries extends AroundEach with EventuallyResults {
  def retries: Int = 5
  def sleep: Duration = 100.millis

  // if the ci server is very loaded the tests might fail, so we retry 5 times
  def around[R : AsResult](r: =>R) =
    AsResult(eventually(retries = retries, sleep = sleep)(r))

}
