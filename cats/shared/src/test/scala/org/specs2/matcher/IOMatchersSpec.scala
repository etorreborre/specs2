package org.specs2
package matcher

import cats.effect._
import cats.implicits._

import scala.concurrent.duration._

class IOMatchersSpec extends mutable.Specification with IOMatchers with ResultMatchers {
  "It is possible to check the execution of tasks" >> {
    "check the return value" >> {
      { IO(1) must returnValue(1) }
      { (IO(1) must returnValue(2)) returns "1 != 2" }
    }

    "check that the task finishes before a given time" >> {
      { IO(1) must returnBefore(10.millis) }
      { (IO.sleep(50.millis).as(1) must returnBefore(10.millis)) returns "Timeout after 10 milliseconds" }
    }

    "check the return value with a timeout" >> {
      { IO.sleep(5.millis).as(1) must returnBefore(10.millis).withValue(1) }
      { IO.sleep(5.millis).as(1) must returnValue(1).before(10.millis) }
    }

    "an IO[MatchResult[_]] is implicitly a Result" >> {
      for {
        x <- IO.pure(42)
      } yield {
        x must_== 42
      }
    }
  }
}
