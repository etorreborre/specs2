package org.specs2
package matcher

import mutable.*
import execute.*
import org.specs2.concurrent.ExecutionEnv
import scala.concurrent.*
import scala.concurrent.duration.*

class EventuallyMatchersSpec(using ee: ExecutionEnv) extends Specification with FutureMatchers with ExpectationsDescription { section("travis")
addParagraph("""
`eventually` can be used to retry any matcher until a maximum number of times is reached
or until it succeeds.
""")

  "A matcher can match right away with eventually" >> {
    1 must eventually(be_==(1))
  }
  "A matcher can match right away with eventually, even if negated" >> {
    "1" must not (beNull.eventually)
  }
  "A matcher will be retried automatically until it matches" >> {
    val iterator = List(1, 2, 3).iterator
    iterator.next must be_==(3).eventually
  }
  "A matcher can work with eventually and be_== but a type annotation is necessary or a be_== matcher" >> {
    val option: Option[Int] = Some(3)
    option must be_==(Some(3)).eventually
  }
  "If all retries fail, the matcher will eventually fail" >> {
    val iterator = LazyList.from(1).iterator
    (iterator.next must be_==(-1).eventually) must throwA[FailureException]
  }
  "It is possible to use aka with eventually (issue #231)" >> {
    var i = 0
    def get = { i += 1; i }
    get `aka` "hello" must beEqualTo(3).eventually
  }
  "It is possible to use await + eventually" >> {
    var i = 0
    def op = if i == 0 then { i += 1; Future(0) } else Future(1)

    op must be_==(1).await(retries = 0, timeout = 200.millis).eventually(retries = 2, sleep = 100.millis)
  }
}
