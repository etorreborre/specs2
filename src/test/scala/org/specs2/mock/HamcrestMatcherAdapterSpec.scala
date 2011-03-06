package org.specs2
package mock

class HamcrestMatcherAdapterSpec extends SpecificationWithJUnit { def is = 

  "A specs2 matcher can be adapted to be used like a Hamcrest matcher"                                                  ^
  "  when the match is ok" ! {
    HamcrestMatcherAdapter(beEqualTo(1)).matchesSafely(1) must beTrue
  }                                                                                                                     ^
  "  when the match is ko" ! {
    HamcrestMatcherAdapter(beEqualTo(1)).matchesSafely(2) must beFalse
  }                                                                                                                     ^
                                                                                                                        p^
  "A specs2 matcher adapted as will add to a Description"                                                               ^
  "  when the match is ok" ! {
    matchMessage(1, 1) must_== ""
  }                                                                                                                     ^
  "  when the match is ko" ! {
    matchMessage(1, 2) must_== "'1' is not equal to '2'"
  }                                                                                                                     ^
                                                                                                                        end
                                                                                          
  def matchMessage(value1: Int, value2: Int) = {
    val m = HamcrestMatcherAdapter(beEqualTo(value2))
    m.matchesSafely(value1)
    val description = new org.hamcrest.StringDescription
    m.describeTo(description)
    description.toString
  }                                                                                          
}