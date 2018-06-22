package org.specs2
package matcher

import cats.effect.IO
import org.specs2.specification.Retries

import scala.concurrent.duration._

class IOMatchersSpec extends mutable.Specification with IOMatchers with ResultMatchers with Retries {
  sequential

  "It is possible to check the execution of tasks" >> {
    "check the return value" >> {
      { IO(1) must returnValue(1) }
      { (IO(1) must returnValue(2)) returns "1 != 2" }
    }

    "check that the task finishes before a given time" >> {
      { IO(1) must returnBefore(10.millis) }
      { (IO { Thread.sleep(50); 1 } must returnBefore(10.millis)) returns "Timeout after 10 milliseconds" }
    }

    "check the return value with a timeout" >> {
      { IO { Thread.sleep(5); 1 } must returnBefore(10.millis).withValue(1) }
      { IO { Thread.sleep(5); 1 } must returnValue(1).before(10.millis) }
    }
  }
}

