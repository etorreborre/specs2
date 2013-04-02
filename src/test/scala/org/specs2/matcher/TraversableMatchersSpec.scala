package org.specs2
package matcher

import specification._
import java.util.Arrays._
import scala.collection.JavaConversions.{ collectionAsScalaIterable }
import scala.collection.parallel.ParSeq
import execute.{FailureException, FailureDetails}

class TraversableMatchersSpec extends Specification with ResultMatchers with Tags with Grouped { def is = s2"""

  we can check if one or several elements are present in a traversable
   ${ List(1, 2) must contain(1) }
   ${ List(1, 2, 3) must contain(3, 2) }
   ${ (List(1, 2, 3) must contain(3, 4)) returns "List(1, 2, 3) doesn't contain '4'" }
   ${ List(1, 2, 3) must containAllOf(List(1, 3)).inOrder }
   ${ List(1, 2, 3, 4, 5) must containAllOf(List(2, 4)).inOrder }
   ${ List(1, 2, 3) must contain(3) and  contain(2) }
    // corner case with type inference. If not specified 'Any', the contain 'String' Matcher is selected
   ${ List("1", "2", "3") must contain("3") and contain("2":Any) }
   ${ "abc" must contain('b') }
   ${ (List(1, 2) must contain(0)) returns "List(1, 2) doesn't contain '0'" }
   with a subclass ${subclass().e1}
   ${ Seq(1, 2, 3, 4) must have oneElementLike  { case i if i > 2 => (i % 2) must_== 0 } }
   ${ Seq(1, 2, 3, 4) must have oneElementLike  { case i if i > 2 => i.toString must haveSize(1) } }
   ${ Seq(1, 2, 3, 4) must have allElementsLike { case i if i > 2 => i must be_>=(1)   } }
   ${ Seq(1, 2, 3, 4) must have allElementsLike  { case i if i > 2 => i.toString must haveSize(1) } }
   ${ (Seq(1, 2, 3, 4) must have oneElementLike { case i if i > 3 => (i % 2) must_== 1 }) returns
      "in List(1, 2, 3, 4)\nno element is correct\n4: '0' is not equal to '1'" }
   ${ (Seq(1, 2, 3, 4) must have allElementsLike { case i if i > 2 => i must be_>=(4)   }) returns
      "in List(1, 2, 3, 4)\nsome elements are not correct\n3: 3 is less than 4" }

    with adapation
    ${ List(1, 2, 3) must contain(4, 3, 2) ^^ ((i: Int, j: Int) => i-j <= 1) }
    ${ List(1, 2, 3) must contain(3, 2, 1) ^^ ((i: Int) => be_<(10-i)) }
    ${ List(1, 2, 3) must contain(3, 2, 1) ^^^ ((_:Int) - 1) }

  we can check if at least one or several elements are present in a traversable
   ${ List(1, 2) must containAnyOf(Seq(1, 4)) }

  we can check the traversable contains another element exactly once
   ${ List(1, 2) must contain(1).exactlyOnce }
   ${ List(1, 1) must contain(1).exactlyOnce.not }

  we can check if an traversable contains other elements in the same order
    ${ List(1, 2, 3, 4) must contain(2, 4).inOrder }
    and fails when one element is missing ${order().fail1}
    or if the order is wrong              ${order().fail2}

  we can check if an traversable has the same elements in the same order, and no more
    ${ List("Hello", "World") must contain("Hello", "World").only.inOrder }
    ${ List("Hello", 1) must contain("Hello", 1) }
    ${ List("Hello", "World") must contain("Hello", "World").inOrder.only }
    ${ List("Hello", "World", "!") must not(contain("Hello", "World").only) }
    ${ List("Hello", "World") must not (contain("World", "Hello").only.inOrder) }
    and show appropriate failure messages if one element doesn't match ${sameSeq().e1}
    with a specific equality function
    ${ List("Hello", "World") must contain("hello", "world").only ^^ ((s1, s2) => s1.toLowerCase == s2.toLowerCase) }
    ${ List("Hello") must not(contain("hello", "world").only ^^ ((s1, s2) => s1.toLowerCase == s2.toLowerCase)) }

  we can check if 2 traversables are contained in each other
    ${ List("1", "2") must containTheSameElementsAs(Seq("2", "1")) }
    ${ { List("1", "2", "3") must containTheSameElementsAs(Seq("2", "4", "1")) } returns Seq(
       "List(1, 2, 3)",
       "  is missing: 4",
       "  must not contain: 3").mkString("\n")
    }
    ${ { List("1", "2", "3") must containTheSameElementsAs(Seq("2", "3", "4", "1")) } returns Seq(
       "List(1, 2, 3)",
       "  is missing: 4").mkString("\n")
    }

  we can use `not contain(1)` in a mutable Scope ${g1.e1}

  we can check the size of an traversable
    ${ Nil must beEmpty }
    ${ Nil must be empty }
    ${ (Nil must not be empty) must beFailing }
    ${ List(1, 2) must haveSize(2) }
    ${ List(1, 2) must have size(2) }
    ${ List(1, 2) must not have size(1) }
    ${ List(1, 2) must haveLength(2) }
    ${ List(1, 2) must have length(2) }
    ${ List(1, 2) must not have length(1) }

  we can check if a sequence is sorted
    ${ Seq(1, 2, 3) must beSorted }
    ${ Seq(1, 2, 3) must be sorted }
    ${ Seq(2, 1, 3) must not beSorted }
    // this doesn't compile because of 'diverging implicit'
    //{ Seq(2, 1, 3) must not be sorted }
    ${ (Seq(2, 1, 3) must beSorted) returns "List(2, 1, 3) is not sorted" }

  we can check the size of an Array
    ${ Array(1, 2) must have size(2) }
    ${ (Array(1, 2) must have size(1)).message must_== "Array(1, 2) doesn't have size 1 but size 2" }

  we can check if a traversable contains elements following a given pattern
    ${ List("Hello", "World") must containMatch("ll") }
    ${ List("Hello", "World") must containPattern(".*llo") }

  we can check if a traversable contains elements with a pattern only once
    ${ List("Hello", "World") must containMatch("ll").onlyOnce }
    ${ List("Hello", "World") must containPattern(".*llo").onlyOnce }
    and fails when there's no match ${patternMatch().fail1}
    or if there's more than one ${patternMatch().fail2}

  we can check if a traversable contains an element with a given property
    ${ List("Hello", "World") must have(_.size >= 5) }
    ${ List("Hello", "World") must not have((_:String).size < 3) }

  we can check if a traversable has the same elements as another one
    ${ List("Hello", "World") must haveTheSameElementsAs(List("Hello", "World")) }
    regardless of the order
    ${ List("Hello", "World") must haveTheSameElementsAs(List("World", "Hello")) }
    with a user-defined equality method
    ${ List("Hello", "World") must haveTheSameElementsAs(List("World", "Hello"), lowerCaseEquality) }
    recursively ${sameElems().e1}
    with a detailed failure message ${sameElems().e2}

    with an adaptation
    ${ List("Hello", "World") must haveTheSameElementsAs(List("W", "H")) ^^^ ((_:String).head) }
    ${ List("Hello", "World") must
       haveTheSameElementsAs(List("ello", "orld")) ^^ ((t1:String, t2: String) => t1.last == t2.last) }
    ${ val beEqualIgnoreCase = be_===(_:String) ^^^ ((_:String).toLowerCase)
      List("Hello", "World") must haveTheSameElementsAs(List("world", "hello")) ^^ beEqualIgnoreCase }
    ${ val startsWitha = (s: String) => be_==("a"+s)
      List("Hello", "World") must haveTheSameElementsAs(List("aWorld", "aHello")) ^^ startsWitha }

  Java collections can also be used with Traversable matchers
  But generally require explicit conversion
    ${ asList("Hello", "World") must haveSize(2) }
    ${ collectionAsScalaIterable(asList("Hello", "World")) must containMatch("ll") }

  Parallel collections work with any matcher
    ${ ParSeq(1, 2, 3) must contain(1, 2, 3) }
                                                                                                                        """

  "edge cases" - new g1 {
    e1 := {
      val spec = new org.specs2.mutable.Specification {
        "ex1" >> new Scope {
          Seq(1) must not contain(1)
        }
      }
      FragmentExecution.executeExamples(spec.content)(args()).head.result must beFailing
    }
  }

  case class subclass() {
    class Food
    case class Pizza() extends Food
    case class Fruit() extends Food
    val diner = List(Pizza(), new Fruit())
    def e1 = diner  must contain(Pizza())
  }
  case class order() {
    def fail1 = (List(1, 2, 3, 4) must contain(2, 5).inOrder) returns 
                "List(1, 2, 3, 4) doesn't contain in order '2, 5'"
    def fail2 = (List(1, 2, 3, 4) must contain(4, 2).inOrder) returns  
                 "List(1, 2, 3, 4) doesn't contain in order '4, 2'"
  }

  case class patternMatch() {
    def fail1 = (List("Hey", "World") must containMatch("llo").onlyOnce) returns
                 "List(Hey, World) doesn't contain match '.*llo.*'"
    def fail2 = (List("Hello", "Bella") must containMatch("ll").onlyOnce) returns
                 "List(Hello, Bella) contains match '.*ll.*' 2 times"
  }
  
  case class sameElems() {
    def e1 = List("Hello", List("Dear", "World"), "!") must haveTheSameElementsAs(List("!", "Hello", List("World", "Dear")))

    def e2 = {
      val result = List("1", List("2", "4"), "3") must haveTheSameElementsAs(List("1", "2", List("5", "3")))
      result must beLike {
        case MatchFailure(_,_,_,FailureDetails(e,a)) => (e === "[1, 2, [5, 3]]") and (a === "[1, [2, 4], 3]")
      }
    }
  }

  case class sameSeq() {
    def e1 = (List("Hello", "World") must contain("Hello2", "World2").inOrder.only) returns
             "List(Hello, World) doesn't contain in order 'Hello2, World2'"
  }

  def lowerCaseEquality = (_:String).toLowerCase == (_:String).toLowerCase
}                                                                                          
