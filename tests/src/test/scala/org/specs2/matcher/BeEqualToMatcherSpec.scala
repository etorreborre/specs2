package org.specs2
package matcher

import execute._

class BeEqualToMatcherSpec extends Specification with ResultMatchers { def is = s2"""

  be_== checks the equality of 2 objects
  ${ "a" must_== "a" }
  ${ "a" must not be_==(null) }
  ${ (null: String) must not be_==("a") }
  ${ "a" must_!= "b" }
  ${ "a" should_== "a" }
  ${ "a" should_!= "b" }
  ${ "a" must be_==("a") }
  ${ "a" must not be_==("b") }
  ${ "a" must be_!=("b") }
  ${ "a" must not be_!=("a") }
  ${ "a" === "a" }
  ${ "a" !== "b" }

  Typed equality
  ${ "a" must be_===("a") }
  // doesn't compile
  // { "a" ==== 1 }
  ${ "a" must not be_===("b") }
  ${ "a" must be_!==("b") }
  ${ "a" must not be_!==("a") }

  Array equality uses deep array comparison
  ${ Array(1, 2) must be_==(Array(1, 2)) }
  ${ Array(1, 3) must not be_==(Array(1, 2)) }
  ${ Array(1, 2) must be_===(Array(1, 2)) }
  ${ Array(Array(1, 2)) must be_===(Array(Array(1, 2))) }
  ${ Array(1, 3) must not be_===(Array(1, 2)) }
  ${ (Array(1, 3) must not be_===(Array(1, 2))) returns
     """|Array(1, 3) is not equal to Array(1, 2)
        |
        |Missing values
        |2
        |
        |Additional values
        |3""".stripMargin }

  Unordered collections (sets and maps) equality uses the BestMatching algorithm
  ${ (Set(1, 2) must be_==(Set(2, 3))) returns
      """|Set(1, 2) is not equal to Set(2, 3)
         |
         |Missing values
         |3
         |
         |Additional values
         |1""".stripMargin
   }

  ${ (Map("cnt" -> 1, "people" -> List(Map("friend" -> Map("name" -> "Bob"), "age" -> 45, "name" -> "Bill")), "city" -> "Montreal") ===
        Map("people" -> List(Map("name" -> "Bill", "friend" -> Map("name" -> "Bob"), "age" -> 45)), "city" -> "Montrea", "cnt" -> 1)) returns
  """|Missing values
     |city -> Montrea
     |
     |Additional values
     |city -> Montreal""".stripMargin
   }

  Other collections use normal equality but display missing elements
  ${ Seq(1, 2) must be_==(Seq(1, 2)) }
  ${ (Seq(1, 2) must be_==(Seq(2, 3))) returns
  """|List(1, 2) is not equal to List(2, 3)
     |
     |Missing values
     |3
     |
     |Additional values
     |1""".stripMargin
  }

  Expected values are kept in the failure details
  ${ (1 must_== 2).toResult must beLike { case Failure(_,_,_,FailureDetails(e, a)) => e must_== "2" } }

  the actual value must be evaluated before the expected one
  ${ var result = "";
     {{ result = result + "a" }; 1} must_== {{ result = result + "b" }; 1}
     result must_== "ab"
  }

Robustness
==========

  the be_== matcher must be robust in face of
    a null object                      $r1
    a non-traversable collection       $r2

Details
=======

  the be_== matcher must warn when comparing 2 objects with the same toString representation but not the same type"
    with List[Int] and List[String]            $d1
    with 'hello': String and 'hello': Hello    $d2
    with List("1, 2") and List("1", "2")       $d3
    with Map(1 -> "2") and Map(1 -> 2)         $d4
"""

  def r1 = ((null: String) must_== "1") must not(throwAn[Exception])

  def r2 = {
    def newTraversable = new TraversableWithNoDefinedForeach[Int] {}
    val (t1, t2) = (newTraversable, newTraversable)
    (t1 must_== t2) must not(throwAn[Exception])
  }

  def d1 = {
    (List(1, 2) must_== List("1", "2")) must beFailing(
     "\\Q'List('1', '2'): scala.collection.immutable.$colon$colon[java.lang.Integer]'\n\n is not equal to \n\n'List('1', '2'): scala.collection.immutable.$colon$colon[java.lang.String]'\\E")
  }

  def d2 = {
    ("hello" must_== Hello()) must beFailing(
        "\\Q'hello: java.lang.String' is not equal to 'hello: org.specs2.matcher.Hello'\\E")
  }
  def d3 = {
    (List("1, 2") must_== List("1", "2")) must beFailing(
        "\\Q'List('1, 2'): scala.collection.immutable.$colon$colon[java.lang.String]'\n\n is not equal to \n\n'List('1', '2'): scala.collection.immutable.$colon$colon[java.lang.String]'\\E")
  }
  def d4= {
    (Map(1 -> "2") must_== Map(1 -> 2)) must beFailing(
        "\\Q'Map(1: java.lang.Integer -> 2: java.lang.String): scala.collection.immutable.Map$Map1'\n\n is not equal to \n\n'Map(1: java.lang.Integer -> 2: java.lang.Integer): scala.collection.immutable.Map$Map1'\\E")
  }

  trait TraversableWithNoDefinedForeach[T] extends Traversable[T] {
    def foreach[U](f: T => U): Unit = {
      sys.error("foreach is not defined on this traversable but toString is")
    }
  }

}
