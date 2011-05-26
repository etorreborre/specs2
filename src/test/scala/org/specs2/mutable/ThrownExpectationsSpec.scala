package org.specs2.mutable
import org.specs2.matcher._

class ThrownExpectationsSpec extends Specification {
  "An acceptance spec using thrown expectations" should {
    "fail when the first matcher fails in an Example" in {
      val spec = new org.specs2.Specification with ThrownExpectations {
        def is = "example" ! { 1 must_== 2; success }
      }
      success
    }
  }
}