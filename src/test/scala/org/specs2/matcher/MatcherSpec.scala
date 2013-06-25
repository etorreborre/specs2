package org.specs2
package matcher

import java.io.File
import specification._
import user.specification._
import io.Location
import org.specs2.execute.Failure

class MatcherSpec extends script.Specification with ResultMatchers with Groups { def is = s2"""

Matchers can be created in different ways

Adaptation
==========

  + a matcher can be adapted with a function
    + the location of match results must be correct after adaptation (#168)
  + a matcher can be adapted with a function and a description function for the expectable
  + if the matcher is for equality, it has to be the typed equality matcher be_===
  + a matcher can be adapted with a function for both expected and actual values
  + a function can be adapted with a matcher to create a matcher

Implicit conversions
====================

  + a matcher can be defined by a function with 1 message
  + a matcher can be defined by a function with 2 messages
  + a matcher can be defined by a function returning a triplet
  + a matcher can be defined by a function returning a pair
  + a matcher can be defined by a function with a function for the ko message
  + a matcher can be defined by a function with 2 functions for the messages
  + a matcher can be muted and will output no message
  + a matcher can be defined by a function returning a MatchResult

Collections
===========

  + a matcher for a seq of values can be defined by a function returning a MatchResult and used forall values+
     meaning that the first failure will fail all
  + a matcher for a seq of values can be defined by a function returning a MatchResult and used foreach values+
     meaning that all failures will be collected

Messages
========

  + a matcher can have a different failure message
                                                                                          """

  "adaptation" - new group {
    eg := new Exception("message")  must be_==("message") ^^ ((_:Exception).getMessage)
    eg := (new UserExpectations).failure1.location must endWith("UserExpectations.scala:11")

    eg := {
      val result = new Exception("message")  must be_>(2) ^^ ((e:Exception) => e.getMessage.size aka "the message size")
      result.message must_== "the message size '7' is greater than 2"
    }
    eg := {
      val result = new Exception("message")  must be_===(8) ^^ ((e:Exception) => e.getMessage.size aka "the message size")
      result.message must_== "the message size '7' is not equal to '8'"
    }
    eg := {
      case class Human(age: Int, wealth: Int)
      def beMostlyEqualTo = (be_==(_:Human)) ^^^ ((_:Human).copy(wealth = 0))
      Human(age = 20, wealth=1000) must beMostlyEqualTo(Human(age = 20, wealth=1)) toResult
    }
    eg := {
      def haveExtension(extension: =>String) = ((_:File).getPath) ^^ endWith(extension)
      new File("spec.scala") must haveExtension(".scala")
    }
  }

  "conversions" - new group {
    eg := {
      def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, "is odd")
      (3 must beEven) returns "'3' is odd"
    }
    eg := {
      def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, "is even", "is odd")
      (3 must beEven) returns "'3' is odd"
    }
    eg := {
      def beEven: Matcher[Int] = (i: Int) => (i % 2 == 0, "is even", "'"+i+"' is odd")
      (3 must beEven) returns "'3' is odd"
    }
    eg := {
      def beEven: Matcher[Int] = (i: Int) => (i % 2 == 0, "'"+i+"' is odd")
      (3 must beEven) returns "'3' is odd"
      (2 must beEven) returns "'2' is not odd"
    }
    eg := {
      def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is odd")
      (3 must beEven) returns "3 is odd"
      (2 must beEven) returns "2 is not odd"
    }
    eg := {
      def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
      (3 must beEven) returns "3 is odd"
    }
    eg := (1 must be_==("1").mute) returns ""

    eg := {
      def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
      def beOdd: Matcher[Int] = ((i: Int) => beEven.apply(theValue(i)).not)
      (2 must beOdd) returns "2 is even"
    }
  }

  "collections" - new group {
    eg := {
      def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
      ((i: Int) => beEven).forall(Seq(1, 2, 3)) returns "1 is odd"
    }

    eg := {
      def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
      ((i: Int) => beEven).foreach(Seq(1, 2, 3)) returns "There are 2 failures\n1 is odd\n3 is odd\n"
    }
  }

  "messages" - new group {
    eg := {
      def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
      (3 must beEven.setMessage("is not even")).message === "is not even"
    }
  }

}