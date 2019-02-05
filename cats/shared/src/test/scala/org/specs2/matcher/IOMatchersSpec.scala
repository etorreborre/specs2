package org.specs2
package matcher

import cats.effect._

class IOMatchersSpec extends mutable.Specification with IOMatchers with ResultMatchers {
  "It is possible to check the execution of tasks" >> {
    "check the return value" >> {
      { IO(1) must returnValue(1) }
      { (IO(1) must returnValue(2)) returns "1 != 2" }
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
