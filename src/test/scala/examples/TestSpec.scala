package examples
import org.specs2._
import execute.{Failure, Success, Result}
import mock._
import matcher._
import specification.Scope

class TestSpec extends mutable.Specification with Mockito {

  "this test" should {

    "test" in {
      val success1: Result = Success("s1")
      val success2 = Success("s2")
      val failure1 = Failure("f1")
      (failure1 and success1) must_== failure1
    }
//    "fail, invocations in wrong order" in  new context {
//      val mockList = mock[List[String]]
//      mockList.apply(1)
//      mockList.apply(0)
//
//      implicit val order = inOrder(mockList)
//      there was one(mockList).apply(2) andThen one(mockList).apply(1)
//    }

  }

  trait context extends Scope with ThrownExpectations {
    def mockList: List[String]
    // appears to be necessary even with one mock
  }
}

