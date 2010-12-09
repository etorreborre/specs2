package org.specs2
package matcher

class EventuallyMatchersSpec extends SpecificationWithJUnit { def is = sequential^
  "matchers can be checked to eventually match after the first try" ! {
    1 must eventually(be_==(1))
  }^
  "matchers can be checked to eventually match after a number of retries" ! {
    val iterator = List(1, 2, 3).iterator
    iterator.next must be_==(3).eventually
  }
}