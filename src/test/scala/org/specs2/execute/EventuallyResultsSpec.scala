package org.specs2
package execute

import mutable.Specification
import mock.Mockito

class EventuallyResultsSpec extends Specification with Mockito {
  """
  `eventually` can be used to retry any result until a maximum number of times is reached
    or until it succeeds.
  """
  "A success succeeds right away with eventually" in {
    eventually(success)
  }
  "A failure will always fail" in {
    eventually(failure).not
  }
  "A result will be retried automatically until it succeeds" in {
    val iterator = List(failure, failure, success).iterator
    eventually(iterator.next)
  }
  "If all retries fail, the result will eventually fail" in {
    lazy val failures: Stream[Result] = failure #:: failures
    val iterator = failures.iterator
    eventually(iterator.next).not
  }
  "Any object convertible to a result can be used with eventually" in {
    val iterator = List(false, false, true).iterator
    eventually(iterator.next) must beTrue
  }
  "eventually can be used for a Mockito result" in {
    trait ToMock {
      def next: Int
    }
    val m = mock[ToMock]
    m.next returns 1 thenReturns 2 thenReturns 3

    eventually(m.next == 3) must beTrue
  }
}
