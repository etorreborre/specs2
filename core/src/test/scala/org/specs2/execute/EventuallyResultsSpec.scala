package org.specs2
package execute

import mutable.Specification
import org.mockito.Mockito._

class EventuallyResultsSpec extends Specification {
  """
  `eventually` can be used to retry any result until a maximum number of times is reached
    or until it succeeds.
  """.txt

  "A success succeeds right away with eventually" in {
    eventually(Success())
  }
  "A failure will always fail" in {
    eventually(Failure()).not
  }
  "A result will be retried automatically until it succeeds" in {
    val iterator = List(Failure(), Failure(), Success()).iterator
    eventually(iterator.next)
  }
  "If all retries fail, the result will eventually fail" in {
    lazy val failures: Stream[Result] = Failure() #:: failures
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
    val m = mock(classOf[ToMock])
    (1 to 3).foreach(i => doReturn(i).when(m).next)
    eventually(m.next == 3) must beTrue
  }
}
