package org.specs2
package matcher

import cats.effect._
import org.specs2.concurrent.ExecutionEnv

class IOMatchersSpec(implicit val ee: ExecutionEnv) extends mutable.Specification with IOMatchers with ResultMatchers {
  "It is possible to check the execution of tasks" >> {

    "an IO[MatchResult[_]] is implicitly a Result" >> {
      for {
        x <- IO.pure(42)
      } yield {
        x must_== 42
      }
    }
  }
}
