package org.specs2
package mock

import specification._
import matcher._
import junit.framework.AssertionFailedError

class MockitoSpec extends script.Specification with Mockito with ResultMatchers with Groups {  def is = s2"""

 The Mockito trait is reusable in other contexts
   + in JUnit
                                                                                                                        """

  "callbacks" - new group {

    eg := {
      val s = new Mockito with JUnitExpectations {
        val list = mock[java.util.List[String]]
        def test = {
          list.add("one")
          there was one(list).add("two")
          1 must_== 1 // to check if the previous expectation really fails
        }
      }
      s.test must throwAn[AssertionFailedError]
    }
  }
}


