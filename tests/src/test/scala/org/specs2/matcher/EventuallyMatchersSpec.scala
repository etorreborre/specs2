package org.specs2
package matcher

import mutable._
import execute._
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global

class EventuallyMatchersSpec extends Spec with FutureMatchers with ExpectationsDescription {
addText { """
`eventually` can be used to retry any matcher until a maximum number of times is reached
or until it succeeds.
""" }
  
  "A matcher can match right away with eventually" in {
    1 must eventually(be_==(1))
  }
  "A matcher can match right away with eventually, even if negated" in {
    "1" must not (beNull.eventually)
  }
  "A matcher will be retried automatically until it matches" in {
    val iterator = List(1, 2, 3).iterator
    iterator.next must be_==(3).eventually
  }
  "A matcher can work with eventually and be_== but a type annotation is necessary or a be_=== matcher" in {
    val option: Option[Int] = Some(3)
    option must be_==(Some(3)).eventually
  }
  "If all retries fail, the matcher will eventually fail" in {
    val iterator = Stream.from(1).iterator
    (iterator.next must be_==(-1).eventually) must throwA[FailureException]
  }
  "It is possible to use aka with eventually (issue #231)" in {
    var i = 0
    def get = { i += 1; i }
    get aka "hello" must beEqualTo(3).eventually
  }
  "It is possible to use await + eventually" in {
    var i = 0
    def op = if (i == 0) { i += 1; Future(0) } else Future(1)

    op must be_==(1).await(retries = 0, timeout = 200.millis).eventually(retries = 1, sleep = 100.millis)
  }

}