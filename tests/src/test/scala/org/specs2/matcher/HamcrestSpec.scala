package org.specs2
package matcher

import org.hamcrest._
import specification._

class HamcrestSpec extends script.Spec with Grouped with Hamcrest with TypedEqual { def is = s2"""

  Hamcrest matchers can be used as specs2 matchers by mixing in the Hamcrest trait      
  + for example a beEven hamcrest matcher can be used in a 'must' expression
    + the failure message must contain the matched value and the hamcrest failure message
                                                                                           """

  new g1 {
    e1 := 2 must beEven
    e2 := (3 must beEven).message === "\nExpected: an even Int\n     but: <3> is odd"
  }

  // a Hamcrest matcher for even numbers
  object beEven extends BaseMatcher[Int] {
    def matches(item: Object): Boolean = item.toString.toInt % 2 == 0
    def describeTo(description: Description): Unit = { description.appendText("an even Int") }
    override def describeMismatch(item: Object, description: Description): Unit = {
      description.appendValue(item).appendText(" is odd")
    }
  }

}
