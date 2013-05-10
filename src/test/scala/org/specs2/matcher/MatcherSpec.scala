package org.specs2
package matcher

import java.io.File

class MatcherSpec extends Specification with ResultMatchers { def is = s2"""
  Matchers can be created in different way
  
  a matcher can be adapted with a function                                                                       $e1
  a matcher can be adapted with a function and a description function for the expectable                         $e1_1
  if the matcher is for equality, it has to be the typed equality matcher be_===                                 $e1_2
  a matcher can be adapted with a function for both expected and actual values                                   $e2
  a function can be adapted with a matcher to create a matcher                                                   $e2_1
  a matcher can be defined by a function with 1 message                                                          $e3
  a matcher can be defined by a function with 2 messages                                                         $e3_1
  a matcher can be defined by a function returning a triplet                                                     $e3_2
  a matcher can be defined by a function returning a pair                                                        $e3_3
  a matcher can be defined by a function with a function for the ko message                                      $e4
  a matcher can be defined by a function with 2 functions for the messages                                       $e5
  a matcher can be muted and will output no message                                                              $e6
  a matcher can be defined by a function returning a MatchResult                                                 $e7
  a matcher for a seq of values can be defined by a function returning a MatchResult and used forall values+
    meaning that the first failure will fail all                                                                 $e8
  a matcher for a seq of values can be defined by a function returning a MatchResult and used foreach values+
    meaning that all failures will be collected                                                                  $e9
  a matcher can have a different failure message                                                                 $e10
                                                                                                                        """

  def e1 = new Exception("message")  must be_==("message") ^^ ((_:Exception).getMessage)
  def e1_1 = {
    val result = new Exception("message")  must be_>(2) ^^ ((e:Exception) => e.getMessage.size aka "the message size")
    result.message must_== "the message size '7' is greater than 2"
  }
  def e1_2 = {
    val result = new Exception("message")  must be_===(8) ^^ ((e:Exception) => e.getMessage.size aka "the message size")
    result.message must_== "the message size '7' is not equal to '8'"
  }

  def e2 = {
    case class Human(age: Int, wealth: Int)
    def beMostlyEqualTo = (be_==(_:Human)) ^^^ ((_:Human).copy(wealth = 0))
    Human(age = 20, wealth=1000) must beMostlyEqualTo(Human(age = 20, wealth=1)) toResult
  }
  def e2_1 = {
    def haveExtension(extension: =>String) = ((_:File).getPath) ^^ endWith(extension)
    new File("spec.scala") must haveExtension(".scala")
  }
  def e3 = {
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, "is odd")
    (3 must beEven) returns "'3' is odd"
  }
  def e3_1 = {
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, "is even", "is odd")
    (3 must beEven) returns "'3' is odd"
  }
  def e3_2 = {
    def beEven: Matcher[Int] = (i: Int) => (i % 2 == 0, "is even", "'"+i+"' is odd")
    (3 must beEven) returns "'3' is odd"
  }
  def e3_3 = {
    def beEven: Matcher[Int] = (i: Int) => (i % 2 == 0, "'"+i+"' is odd")
    (3 must beEven) returns "'3' is odd"
    (2 must beEven) returns "'2' is not odd"
  }
  def e4 = {
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is odd")
    (3 must beEven) returns "3 is odd"
    (2 must beEven) returns "2 is not odd"
  }
  def e5 = {
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
    (3 must beEven) returns "3 is odd"
  }
  def e6 = (1 must be_==("1").mute) returns ""

  def e7 = {
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
    def beOdd: Matcher[Int] = ((i: Int) => beEven.apply(theValue(i)).not)
    (2 must beOdd) returns "2 is even"
  }

  def e8 = {
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
    ((i: Int) => beEven).forall(Seq(1, 2, 3)) returns "1 is odd"
  }

  def e9 = {
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
    ((i: Int) => beEven).foreach(Seq(1, 2, 3)) returns "There are 2 matches\n1 is odd\n3 is odd\n"
  }

  def e10 = {
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i+" is even", (i: Int) => i+" is odd")
    (3 must beEven.setMessage("is not even")).message === "is not even"
  }

}