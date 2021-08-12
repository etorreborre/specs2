package org.specs2
package matcher

import mutable.*
import execute.FailureException

class ExpectableSpec extends Spec with ResultMatchers with MustMatchers:

  "An expectable can have a precise description with aka(description)" >> {
    ("a" `aka` "the string").description must ===("the string 'a'")
  }

  "If it is a boolean its value is not displayed, only the description" >> {
    (true `aka` "my boolean").description must ===("my boolean")
  }

  "An expectable described with aka only will take its own toString value as the description" >> {
    ("a" aka).description must ===("a 'a'")
  }

  "An expectable described with post will have some description text appended to its toString value" >> {
    ("a" `post` "is the first letter").description must ===("a is the first letter")
  }

  "An expectable can be described with a function taking its toString value" >> {
    ("b" `as` ((s: String) => "a" + s + "c")).description must ===("abc")
  }

  "An expectable can be described with a function describing its value" >> {
    (List(1, 2) `showAs` ((_: Seq[Int]).mkString("|")) must haveSize(3)) returns "'1|2' doesn't have size 3 but size 2"
  }

  "An expectable can be mapped to another value, keeping its ability to throw exceptions when not matching" >> {
    val factory = new ThrownExpectations() {}
    (factory.createExpectable("a").map(1) must ===(2)) must throwA[FailureException]
  }

  "An expectable must return an error when applied a null matcher" >> {
    ("hello" must (null: Matcher[String])) must throwAn[IllegalArgumentException].like { case e =>
      e.getMessage must startWith("You cannot use a null matcher on 'hello'")
    }
  }

  "An expectable must match without an exception if the underlying value returns null for its toString value" >> {
    case class NullString():
      override def toString = null
    NullString() must ===(NullString())
  }

  "the description of an Expectable can be updated with another description" >> {
    ("a" `aka` "the string").updateDescription(_ + "!").description === "the string 'a'!"
  }

  trait ListOf[T]
