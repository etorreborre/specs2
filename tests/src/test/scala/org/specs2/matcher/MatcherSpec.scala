package org.specs2
package matcher

import java.io.File
import user.specification.*
import execute.*
import MatcherImplicits.*

class MatcherSpec extends Specification { def is = s2"""

Matchers can be created in different ways

Adaptation
==========

  a matcher can be adapted with a function $adapt1
    the location of match results must be correct after adaptation (#168) $adapt2
    the match result expectable must be set correctly $adapt3
  a matcher can be adapted with a function and a description function for the expectable $adapt4
  if the matcher is for equality, it has to be the typed equality matcher be_== $adapt5
  a matcher can be adapted with a function for both expected and actual values $adapt6
  the adapted matcher must show both original and adapted values $adapt7
  a function can be adapted with a matcher to create a matcher $adapt8
  a matcher can be adapted with an expectable $adapt9

Implicit conversions
====================

  a matcher can be defined by a function with 1 message $convert1
  a matcher can be defined by a function returning a pair $convert2
  a matcher can be defined by a function with a function for the ko message $convert3
  a matcher can be muted and will output no message $convert4
  a matcher can be defined by a function returning a Result $convert5
  when a matcher is defined by a function returning a Result, it must keep its failure details $convert6
  failure details can be collected when doing a forall check on a collection $convert7

Collections
===========

  a matcher for a seq of values can be defined by a function returning a Result and used forall values
    meaning that the first failure will fail all $collection1
  a matcher for a seq of values can be defined by a function returning a Result and used foreach values
    meaning that all failures will be collected $collection2

Messages
========

 a matcher can have a different failure message $messages1
 a specific messages can be set on a mutable matcher $messages2

"""

  def adapt1 =
    new Exception("message") must be_==("message") ^^ ((_:Exception).getMessage)

  def adapt2 =
    (new UserExpectations).failure1.location must endWith("UserExpectations.scala:11")

  def adapt3 =
    val expectable: Expectable[Exception] = theValue(new Exception("message"))
    expectable.must(be_==("message") ^^ ((_:Exception).getMessage))

  def adapt4 =
    val result = new Exception("message") must be_<(2) ^^ ((e:Exception) => e.getMessage.length `aka` "the message size")
    result.message must ===("the message size '7' is greater than 2")

  def adapt5 =
    val result = new Exception("message") must be_==(8) ^^ ((e:Exception) => e.getMessage.length `aka` "the message size")
    result.message must ===("the message size '7 != 8'")

  def adapt6 =
    case class Human(age: Int, wealth: Int)
    def beMostlyEqualTo: Human => Matcher[Human] =
      ((expected: Human) => (actual: Human) => actual must be_==(expected) ^^^ ((_:Human).copy(wealth = 0)))

    Human(age = 20, wealth=1000) must beMostlyEqualTo(Human(age = 20, wealth=1))

  def adapt7 =
    def beEqualTrimmed: String => Matcher[String] =
      ((expected: String) => (actual: String) => actual must be_==(expected) ^^^ ((_:String).trim))

    val message = (" abc" must beEqualTrimmed("abc   ")).message
    (message must contain(" abc")) and
    (message must contain("abc  ")) and
    (message must contain("abc"))

  def adapt8 =
    def haveExtension(extension: =>String): Matcher[File] =
      ((_:File).getPath) ^^ endWith(extension)

    new File("spec.scala") must haveExtension(".scala")

  def adapt9 =
    def beThree: Matcher[Int] = be_==(3)
    val beStringThree = beThree ^^ ( (_: String).toInt `aka` s"the value")
    ("4" must beStringThree).message === "the value '4 != 3'"

  def adapt10 =
    def beThree: Matcher[Int] = be_==(3)
    val beStringThree = beThree ^^ ( (_: String).toInt `aka` s"the value")
    ("3" must beStringThree).message === "the value '3' is equal to '3'"

  def convert1 =
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, "is odd")
    (3 must beEven) returns "'3' is odd"

  def convert2 =
    def beEven: Matcher[Int] = (i: Int) => (i % 2 == 0, "'"+i.toString+"' is odd")
    ((3 must beEven) returns "'3' is odd") and
    (2 must beEven)

  def convert3 =
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i.toString+" is odd")
    ((3 must beEven) returns "3 is odd") and
    (2 must beEven)

  def convert4 =
    (1 must be_==(1).mute) returns ""

  def convert5 =
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i.toString+" is odd")
    def beOdd: Matcher[Int] = ((i: Int) => i % 2 != 0, (i: Int) => i.toString+" is even")
    (2 must beOdd) returns "2 is even"

  def convert6 =
    def beOneTwoThreeList: Matcher[List[Int]] = (list: List[Int]) => list must be_==(List(1, 2, 3))
    val result = List(1, 2) must beOneTwoThreeList
    result must beLike { case f: Failure => f.details must ===(FailureSeqDetails(List(1, 2), List(1, 2, 3))) }

  def convert7 =
    val result = forallWhen(List(1, 2)) { case i if i == 1 => List(1) must be_==(List(2)) }
    result must beLike { case f: Failure => f.details must ===(FailureSeqDetails(List(1), List(2))) }

  def collection1 =
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i.toString+" is odd")
    forall(Seq(1, 2, 3))((i: Int) => i must beEven) returns ("1 is odd")

  def collection2 =
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i.toString+" is odd")
    foreach(Seq(1, 2, 3))((i: Int) => i must beEven) returns "There are 2 failures\n1 is odd\n3 is odd\n"

  def messages1 =
    def beEven: Matcher[Int] = ((i: Int) => i % 2 == 0, (i: Int) => i.toString+" is odd")
    (3 must beEven.setMessage("is not even")).message === "is not even"

  def messages2 =
    class Spec1() extends org.specs2.mutable.Specification:
      def test: Result =
        true must beFalse.setMessage("is not ok")
    ResultExecution.execute(Spec1().test).message === "is not ok"

}
