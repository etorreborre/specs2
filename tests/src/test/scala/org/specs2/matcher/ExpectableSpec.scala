package org.specs2
package matcher
import mutable._
import execute.FailureException

class ExpectableSpec extends Specification with ResultMatchers with org.specs2.mock.Mockito {

  "An expectable can have a precise description with aka(description)" in {
    ("a" aka "the string").description must_== "the string 'a'"
  }
  "An expectable described with aka will only evaluate the description in case of a failure" in {
    var evaluated = false
    ("a" aka {
      evaluated = true
      "the string" }) must_== "a"
    "the aka description is not evaluated on a success" <==> { evaluated === false }
  }
  "If it is a boolean its value is not displayed, only the description" in {
    (true aka "my boolean").description must_== "my boolean"
  }
  "An expectable described with aka only will take its own toString value as the description" in {
    ("a" aka).description must_== "a 'a'"
  }
  "An expectable described with post will have some description text appended to its toString value" in {
    ("a" post "is the first letter").description must_== "a is the first letter"
  }
  "An expectable can be described with a function taking its toString value" in {
    ("b" as ((s:String) => "a"+s+"c")).description must_== "abc"
  }
  "An expectable can be described with a function describing its value" in {
    (Seq(1, 2) showAs((_:Seq[Int]).mkString("|")) must haveSize(3)) returns "1|2 doesn't have size 3 but size 2"
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
  "A Traversable expectable can be described if" >> {
    "if it has a valid toString method" >> {
      trait Trav[T] extends Traversable[T] {
        override def toString = "trav"
        def foreach[U](f: T => U): Unit = ()
      }
      theValue(new Trav[Int] {}).description === "trav"
    }
  }
  "An expectable must match without an exception on a mock" in {
    val l = mock[List[Int]]
    l must be_==(l)
  }
  "the description of an Expectable can be updated with another description" in {
    ("a" aka "the string").updateDescription(_ + "!").description === "the string 'a'!"
  }
}