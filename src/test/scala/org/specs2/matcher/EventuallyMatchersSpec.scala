package org.specs2
package matcher

class EventuallyMatchersSpec extends SpecificationWithJUnit { def is = sequential   ^
                                                                                              """
   `eventually` can be used to retry any matcher until a maximum number of times is reached
   or until it succeeds.
                                                                                              """^
  p^
  "A matcher can match right away with eventually" ! {
    1 must eventually(be_==(1))
  }^
  "A matcher will be retried automatically until it matches" ! {
    val iterator = List(1, 2, 3).iterator
    iterator.next must be_==(3).eventually
  }
  "If all retries fail, the matcher will eventually fail" ! {
    val iterator = List(1, 2, 3).iterator
    (iterator.next must be_==(-1).eventually).isSuccess must beFalse
  }
}