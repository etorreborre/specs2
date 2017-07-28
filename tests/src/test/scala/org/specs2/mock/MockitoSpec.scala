package org.specs2
package mock

import specification._
import matcher._
import junit.framework.AssertionFailedError

class MockitoSpec extends script.Spec with Mockito with ResultMatchers with Groups {  def is = s2"""

  Contexts
  ========

  The Mockito trait is reusable in other contexts
   + in JUnit

 Issues
 ======
 
 The following mockito issues are fixed
   + #584

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

  "issues" - new group {
    eg := {
      trait Mockable { def method(list: List[String]): String }
      lazy val m = mock[Mockable]

      m.method(===(List[String]())).returns("Hello1")
      m.method(===(List[String]("2"))).returns("Hello2")
      m.method(List()) === "Hello1"
      m.method(List("2")) === "Hello2"
    }
  }
}


