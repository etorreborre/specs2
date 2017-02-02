package org.specs2
package matcher

import mutable.Specification
import execute._

class MatchersImplicitsSpec extends Specification with ResultMatchers {
  "A sequence of match results can be implictly transformed to a single result" >> {
    (List.empty[MatchResult[Any]]: Result) must beSuccessful
    ResultExecution.execute(Seq(1 === 1, 2 === 3): Result) must beFailing
  }

  "A matcher can be built from a function returning a MatchResult" >> {
    val beZero: Matcher[Int] = (i: Int) => { i must_== 0 }
    1 must not(beZero)
  }

  "2 nested foralls must throw an Error if the inner one throws an Error" >> {
    def nested =
      forall(Seq(1)) { i =>
        forall(Seq(2)) { j =>
          sys.error("boom"); true
        }
      }
    nested must throwAn[ErrorException]("boom")
  }

  "An implicit matcher defined with a Result must retain its stacktrace" >> {

    val beZero: Matcher[Int] = { i: Int =>
      if (i != 0) Failure(s"must be 0, got $i", "0", exception.getStackTrace.toList) else Success("ok")
    }

    AsResult(1 must beZero) must beLike {
      case Failure(message, _, trace, _) =>
        message ==== "must be 0, got 1"
        trace ==== exception.getStackTrace.toList

      case other => ko("got "+other)
    }

  }

  val exception = new Exception

}
