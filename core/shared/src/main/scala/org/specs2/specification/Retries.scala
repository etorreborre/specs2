package org.specs2
package specification

import org.specs2.execute.{AsResult, EventuallyResults}
import EventuallyResults._
import scala.concurrent.duration._

trait Retries extends AroundEach {
  def retries: Int = 5
  def sleep: Duration = 100.millis

  // if the ci server is very loaded the tests might fail, so we retry 5 times
  def around[R : AsResult](r: =>R) =
    AsResult(eventually(retries = retries, sleep = sleep)(r))

}
