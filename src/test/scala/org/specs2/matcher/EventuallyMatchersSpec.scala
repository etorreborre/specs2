package org.specs2
package matcher
import mutable._
import execute._

class EventuallyMatchersSpec extends SpecificationWithJUnit {
                                                                                                                        """
`eventually` can be used to retry any matcher until a maximum number of times is reached
or until it succeeds.
                                                                                                                        """
  "A matcher can match right away with eventually" in {
    1 must eventually(be_==(1))
  }
  "A matcher will be retried automatically until it matches" in {
    val iterator = List(1, 2, 3).iterator
    iterator.next must be_==(3).eventually
  }
  "A matcher can work with eventually and be_== but a type annotation is necessary or a be_=== matcher" in {
    val option: Option[Int] = Some(3)
    option must be_===(Some(3)).eventually
  }
  "If all retries fail, the matcher will eventually fail" in {
    val iterator = Stream.from(1).iterator
    (iterator.next must be_==(-1).eventually) must throwA[FailureException]
  }
}