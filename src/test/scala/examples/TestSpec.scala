package examples

import org.specs2.mutable.Specification
import org.specs2.mock.Mockito

class TestSpec extends Specification with Mockito {

  trait ToMock {
    def fn(arg1: =>String): Any
  }
  val m = mock[ToMock]
  "try that" >> {
    m.fn("string")
    there was one(m).fn(be_===("string"))
  }
}
