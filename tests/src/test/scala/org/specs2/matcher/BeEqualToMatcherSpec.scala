package org.specs2
package matcher

import execute._

class BeEqualToMatcherSpec extends Spec with ResultMatchers with ShouldMatchers { def is = s2"""

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
  ${ "a" ==== "a" }
  ${ "a" must not(be_===("b")) }
  ${ "a" must be_!==("b") }
  ${ "a" must_=== "a" }
  ${ "a" must_!== "b" }
  // doesn't compile
  // { "a" ==== 1 }
  ${ "a" must not be_===("b") }
  ${ "a" must not be_==("b") }
  ${ "a" must be_!==("b") }
  ${ "a" must not be_!==("a") }

  Array equality uses deep array comparison
  ${ Array(1, 2) must be_==(Array(1, 2)) }
  ${ Array(1, 3) must not be_==(Array(1, 2)) }
  ${ Array(1, 2) must be_===(Array(1, 2)) }
  ${ Array(Array(1, 2)) must be_===(Array(Array(1, 2))) }
  ${ Array(1, 3) must not be_===(Array(1, 2)) }
  ${ (Array(1, 3) must not be_===(Array(1, 2))) returns
     """Array(1, 3 != 2)""" }

  Set equality
  ${ (Set(1) must_== Set.empty[Int]) returns "Set(1) != Set()"}
  ${ (Set(1, 2) must be_==(Set(2, 3))) returns
      """Set(1, 2) != Set(2, 3)""" }
  ${ (Set(1, 2) must be_===(Set(2, 3))) returns
      """Set(2, added: 3, removed: 1)""" }

  Map equality
  ${ Map(1 -> 2, 3 -> 4) must be_==(Map(3 -> 4, 1 -> 2)) }
  ${ Map(1 -> 2, 3 -> 4) must be_===(Map(3 -> 1, 1 -> 4)) returns
      """Map(1 -> {2 != 4}, 3 -> {4 != 1})""" }

  Other collections use normal equality but display missing elements
  ${ Seq(1, 2) must be_==(Seq(1, 2)) }
  ${ (Seq(1, 2) must be_==(Seq(2, 3))) returns """List(1, 2) != List(2, 3)""" }
  ${ Seq(1, 2) must be_===(Seq(1, 2)) }
  ${ Seq(1, 2) must be_===(Seq(2, 3)) returns """List(1 != 2, 2 != 3)""" }

  Expected values are kept in the failure details
  ${ (1 must_== 2).toResult must beLike { case Failure(_,_,_,FailureDetails(a, e)) => e must_== "2" } }

  the actual value must be evaluated before the expected one
  ${ var result = "";
     {{ result = result + "a" }; 1} must_== {{ result = result + "b" }; 1}
     result must_== "ab"
  }

Robustness
==========

  the be_== matcher must be robust in face of
    a null object                      $r1
    a null object                      $r11
    a non-traversable collection       $r2
    a non-traversable collection       $r22

Details
=======

  the be_== matcher must warn when comparing 2 objects with the same toString representation but not the same type"
    with List[Int] and List[String]            $d1
    with 'hello': String and 'hello': Hello    $d2
    with List("1, 2") and List("1", "2")       $d3
    with Map(1 -> "2") and Map(1 -> 2)         $d4
"""

  def r1 = ((null: String) must_== "1") must not(throwAn[Exception])
  def r11 = (null: String) must be_===("1") must not(throwAn[Exception])

  def r2 = {
    def newTraversable = new TraversableWithNoDefinedForeach[Int] {}
    val (t1, t2) = (newTraversable, newTraversable)
    (t1 must_== t2) must not(throwAn[Exception])
  }

  def r22 = {
    def newTraversable = new TraversableWithNoDefinedForeach[Int] {}
    val (t1, t2) = (newTraversable, newTraversable)
    t1 must be_===(t2) must not(throwAn[Exception])
  }

  def d1 = List(1, 2) must be_===( List("1", "2") ) must beFailing( "\\QList(1 != '1', 2 != '2')\\E" )

  def d2 = {
    ("hello" must_== Hello()) must beFailing(
        "\\Qhello: java.lang.String != hello: org.specs2.matcher.Hello\\E")
  }

  def d3 = { List("1, 2") must be_===( List("1", "2") ) must beFailing( "\\QList('1, 2' != '1', added: '2')\\E" ) }

  def d4= { Map(1 -> "2") must be_===( Map(1 -> 2) ) must beFailing( "\\QMap(1 -> {'2' != 2})\\E" ) }

  trait TraversableWithNoDefinedForeach[T] extends Traversable[T] {
    def foreach[U](f: T => U): Unit = {
      sys.error("foreach is not defined on this traversable but toString is")
    }
  }
}
