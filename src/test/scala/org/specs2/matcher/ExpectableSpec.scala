package org.specs2
package matcher
import mutable._
import execute.FailureException

class ExpectableSpec extends Specification with ResultMatchers with org.specs2.mock.Mockito {

  "An expectable can have a precise description with aka(description)" in {
    ("a" aka "the string").description must_== "the string 'a'"
  }
  "If it is a boolean its value is not displayed, only the description" in {
    (true aka "my boolean").description must_== "my boolean"
  }
  "An expectable described with aka only will take its own toString value as the description" in {
    ("a" aka).description must_== "a 'a'"
  }
  "An expectable described with post will have some description text appended to its toString value" in {
    ("a" post " is the first letter").description must_== "a is the first letter"
  }
  "An expectable can be described with a function taking its toString value" in {
    ("b" as ((s:String) => "a"+s+"c")).description must_== "abc"
  }
  "An expectable can be mapped to another value, keeping its ability to throw exceptions when not matching" in {
    val factory = new ThrownExpectations () {}
    (factory.createExpectable("a").map(1) must_== 2) must throwA[FailureException]
  }
  "An expectable must return an error when applied a null matcher" in {
    ("hello" must (null: Matcher[String])) must throwAn[IllegalArgumentException].like { case e =>
      e.getMessage must startWith("You cannot use a null matcher on 'hello'")
    }
  }
  "An expectable must match without an exception if the underlying value returns null for its toString value" in {
    case class NullString() {
      override def toString = null
    }
    NullString() must_== NullString()
  }
  "An expectable must match without an exception on a mock" in {
    1 must beNone
    val l = mock[List[Int]]
    l must be_==(l)
  }
}