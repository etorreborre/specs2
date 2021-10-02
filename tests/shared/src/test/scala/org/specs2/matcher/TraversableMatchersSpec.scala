package org.specs2
package matcher

import java.util.Arrays.*
import scala.collection.JavaConverters.*
import org.specs2.specification.core.{Env, OwnEnv}

class TraversableMatchersSpec(val env: Env) extends Specification with ResultMatchers with MustMatchers with OwnEnv {
  def is = s2"""

 We can check the elements of a collection by using matchers
   ${Seq(1, 2, 3) must contain(2)}
   ${Seq(1, 2, 3) must contain(be_>=(2))}
   ${Seq(1, 2, 3) must not(contain(be_>=(4)))}
   ${Seq(1, 2, 3) must contain(be_>=(2)).atLeastOnce}
   ${Seq(1, 2, 3) must contain(be_>=(2)).exactly(2.times)}
   ${Seq(1, 2, 3) must contain(be_>=(2)).atMost(2.times)}
   ${Seq(1, 2, 3) must contain(be_>=(2)).atLeast(1.times)}
   ${Seq(1, 2, 3) must contain(be_>=(2)).between(1.times, 2.times)}
   ${Seq(1, 2, 3) must contain(be_>=(0)).forall}
   ${Seq(1, 2, 3) must contain(be_>=(0)).foreach}
   ${Seq(1, 2) must contain(anyOf(1, 4))}
   ${(Seq(1, 2, 3) must not(contain(anyOf(1, 2, 4)))) returns "Expectation failed:\nThere is 1 failure\n'3' is not contained in '1, 2, 4'\n"}

   ${Seq("hello", "world") must contain(matching(".*orld"))}
   ${Seq("hello", "world") must contain((s: String) => s.length > 2)}
   ${Seq("1", "2", "3") must (contain(===("3")) and contain("2"))}
   ${Seq("foobar").must(contain("foo")).not} see #416
   ${Seq[Food](Pizza(), new Fruit()) must contain(Pizza())}
   a string inclusion must work as well  ${"abc" must contain('b')}


   Failure messages
   ${(Seq(1, 2, 3) must contain(4)) returns "List(1, 2, 3) does not contain 4"}
   ${(Seq(1, 2, 3) must contain(be_>=(4))) returns "There are 3 failures\n1 is strictly less than 4\n2 is strictly less than 4\n3 is strictly less than 4\n"}
   ${(Seq(1, 2, 3) must not(contain(be_>=(2)))) returns "Expectation failed:\nThere is 1 failure\n1 is strictly less than 2"}
   ${(Seq(1, 2, 3) must contain(be_>=(3))
    .atLeast(2.times)) returns "There are 2 failures\n1 is strictly less than 3\n2 is strictly less than 3\n"}

 We can compare a collection to another by using matchers

   ${Seq[Int]() must contain(exactly(Seq[Int]().map(valueIsTypedValueCheck)*))}
   ${Seq(1, 2, 3) must contain(exactly(1, 2, 3))}
   ${(Seq(1, 2, 3) must contain(exactly(1, 2, 3, 4))) returns "List(1, 2, 3) does not contain 4"}
   ${Seq(1, 2, 3) must contain(exactly(be_>=(0), be_>=(1), be_>=(2)))}
   ${Seq(1, 2, 3) must contain(atLeast(be_>=(0), be_>=(1), be_<=(1)))}
   ${Seq(1, 2, 3) must contain(exactly(be_>=(0), be_>=(1), be_>=(2)).inOrder)}
   // this must be understood as allOf(2, 3)
   ${Seq(1, 2, 3) must contain(2, 3)}
   ${Seq(1) must contain(allOf(1))}
   ${Seq(2) must not(contain(allOf(1)))}
   ${Seq(1) must contain(allOf(1, 1))}
   ${Seq(1) must contain(eachOf(1))}
   ${Seq(2) must not(contain(eachOf(1)))}
   ${Seq(1) must not(contain(eachOf(1, 1)))}
   ${Seq(1, 1) must contain(eachOf(1, 1))}
   ${Seq(1, 1, 2) must contain(allOf(1, 1)).inOrder} $xtag
   ${Seq(1) must contain(allOf(List[Int]()*))}
   ${Seq[Int]() must contain(atMost(1))}
   ${Seq[Int]() must not(contain(allOf(===(1))))}
   ${(Seq(1) must contain(eachOf(1, 1))) returns "List(1) is missing the value: 1"}
   ${Seq(1, 2, 3) must contain(allOf(Seq(1, 2).map(be_>=(_))*))}
   ${Seq(1, 2, 3) must contain(allOf(Seq(1, 2).map(be_>=(_))*)).inOrder}
   ${Seq(1, 2, 3) must not(contain(allOf(Seq(0, 0).map(be_<=(_))*)))}
   ${Seq(1, 2, 3) must contain(allOf(1, 3).inOrder)}
   ${Seq(1, 1, 3) must contain(allOf(1, 3).inOrder)}
   ${(Seq(1) must contain(eachOf(1, 1).inOrder)) returns "List(1) is missing the value: 1"}
   ${Seq(1, 2, 3) must contain(atLeast(Seq[Int]().map(valueIsTypedValueCheck)*))}
   ${Seq(1, 2, 3) must contain(atLeast(3, 1))}
   ${Seq(1, 2, 3) must contain(atLeast(be_>=(0), be_>=(1), be_>=(2)))}
   ${Seq(1, 2) must contain(atMost(2, 1, 3))}
   ${Seq(1, 2, 3) must contain(atMost(be_>=(0), be_>=(1), be_>=(2)))}
   ${Seq(1, 2, 3) must contain(atMost(be_>=(0), be_>=(1), be_>=(2)).inOrder)}

   Failure messages
   ${(Seq[Int]() must contain(2, 3)) returns "List() does not contain 2, 3"}
   ${(Seq[Int]() must contain(allOf(2, 3))) returns "List() does not contain 2, 3"}
   ${(Seq[Int](1, 2, 3) must contain(allOf(2, 5))) returns "List(1, 2, 3) does not contain 5\nFailures:\n  3: 3 != 5\n  1: 1 != 5"}
   ${(Seq[Int]() must contain(exactly(1))) returns "List() does not contain 1"}
   ${(Seq(1, 2, 3) must contain(exactly(1, 2))) returns "List(1, 2, 3) contains 3"}
   ${(Seq(1, 2, 3) must contain(exactly(be_>=(0), be_>=(1), be_>=(5)))) returns
    "List(1, 2, 3) does not contain exactly 3 correct values\n" +
    "- 3\n" +
    " * 3 is strictly less than 5\n"}
   ${(Seq(1, 2, 3) must contain(allOf(3, 2).inOrder)) returns "the value 2 is missing or not in order"}
   ${(Seq(1, 2, 3) must contain(exactly(be_>=(0), be_>=(2), be_<=(1)).inOrder)) returns
    "List(1, 2, 3) does not contain exactly 3 correct values in order\n" +
    "- 3\n" +
    " * 3 is strictly greater than 1\n"}

   ${(Seq(1, 2, 3) must contain(atLeast(4, 1))) returns "List(1, 2, 3) does not contain 4"}
   ${(Seq(1, 2) must contain(atMost(1, 3))) returns "List(1, 2) does not contain 3\nFailures:\n  2: 2 != 3"}
   ${(Seq(1, 2) must contain(atMost(1))) returns "List(1, 2) contains 2"}

   ${(Seq(1, 2, 3) must contain(exactly(1, 2, 3, 4))) returns "List(1, 2, 3) does not contain 4"}

   TODO: from scala 2.13.0-M5 the order is different, why?
   $${ (Seq(1, 2, 3) must contain(exactly(1, 4))) returns "List(1, 2, 3) does not contain 4 but contains 3, 2" }

   ${(Seq(1, 2, 3) must contain(exactly(1, 2, 3, 4, 5))) returns "List(1, 2, 3) does not contain 4, 5"}
   ${(Seq(1, 2, 3) must contain(atLeast(1, 2, 3, 4))) returns "List(1, 2, 3) does not contain 4"}
   ${(Seq(1, 2, 3) must contain(atLeast(1, 2, 3, 4, 5))) returns "List(1, 2, 3) does not contain 4, 5"}
   ${(Seq(1, 2, 3) must contain(atMost(1, 2))) returns "List(1, 2, 3) contains 3"}

   Negation
   ${Seq(1, 2) must not(contain(exactly(1, 2, 3)))}
   ${Seq[Int]() must not(contain(exactly(1)))}
   ${Seq(1, 2, 3) must not(contain(exactly(1, 2)))}
   ${Seq(1, 2) must not(contain(atLeast(4, 1)))}
   ${Seq(1, 2) must not(contain(atMost(1, 3)))}
   ${Seq(1, 2) must not(contain(atMost(1)))}

   ${(Seq(1, 2) must not(contain(exactly(1, 2)))) returnsResult "failure: Expectation failed:\nList(1, 2) contains all expected values"}
   ${(Seq[Int]() must not(contain(exactly(1))))}
   ${(Seq(1, 2, 3) must not(contain(exactly(1, 2))))}
   ${(Seq(1, 2) must not(contain(atLeast(4, 1))))}
   ${(Seq(1, 2) must not(contain(atMost(1, 3))))}
   ${(Seq(1, 2) must not(contain(atMost(1))))}
   ${(Seq(1, 2) must not(contain(allOf(1, 2)))) returnsResult "failure: Expectation failed:\nList(1, 2) contains all expected values"}
   ${(Seq(1, 2, 3) must not(contain(exactly(1, 2, 3, 4))))}

   ${(Seq(1, 2, 3) must not(contain(exactly(1, 4))))}
   ${(Seq(1, 2, 3) must not(contain(exactly(1, 2, 3, 4, 5))))}
   ${(Seq(1, 2, 3) must not(contain(atLeast(1, 2, 3, 4))))}
   ${(Seq(1, 2, 3) must not(contain(atLeast(1, 2, 3, 4, 5))))}
   ${(Seq(1, 2, 3) must not(contain(atMost(1, 2))))}

   we can check if a traversable contains elements following a given pattern
   ${Seq("Hello", "World") must containMatch("ll")}
   ${Seq("Hello", "World") must containPattern(".*llo")}


 Size
 ===

 We can check the size of an traversable
   ${Seq() must beEmpty}
   ${(Seq() must not(beEmpty)) must beFailing}
   ${Seq(1, 2) must haveSize(2)}
   ${Seq(1, 2) must haveSize(be_>=(1))}
   ${Seq(1, 2) must not(haveSize(be_<=(0)))}
   ${Seq(1, 2) must haveLength(2)}

 We can check the size of an Array
    ${Array(1, 2) must haveSize(2)}
    ${(Array(1, 2) must haveSize(1)).message must ===("'Array(1, 2)' doesn't have size 1 but size 2")}

 Sorting
 =======

 We can check if a sequence is sorted
    ${Seq(1, 2, 3) must beSorted}
    ${Seq(2, 1, 3) must not(beSorted)}
    // this doesn't compile because of 'diverging implicit'
    //{ Seq(2, 1, 3) must not(beSorted) }
    ${(Seq(2, 1, 3) must beSorted) returns "List(2, 1, 3) is not sorted"}

 Compare to another traversable
 ==============================

  We can check if 2 traversables are contained in each other
   ${List("1", "2") must containTheSameElementsAs(Seq("2", "1"))}
   ${{ List("1", "2", "3") must containTheSameElementsAs(Seq("2", "4", "1")) } returns Seq(
    "List(1, 2, 3)",
    "  is missing: 4",
    "  must not contain: 3"
  ).mkString("\n")}
   ${{ List("1", "2", "3") must containTheSameElementsAs(Seq("2", "3", "4", "1")) } returns Seq(
    "List(1, 2, 3)",
    "  is missing: 4"
  ).mkString("\n")}

  A user-defined equality function can also be specified ${
    val equality: (A, A) => Boolean = (a1: A, a2: A) => a1.i == a2.i
    Seq(A(i = 1), A(i = 2)) must containTheSameElementsAs(Seq(A(i = 2, j = 2), A(i = 1, j = 2)), equality)
  }

  type annotations might be necessary in some cases ${ // otherwise "could not find implicit value for evidence parameter of type org.specs2.execute.AsResult[A]"
  Seq(Seq(A(1))) must contain(exactly[Seq[A]](Seq(A(1))))}

 With Java collections
 =====================

 Java collections can also be used with Traversable matchers but generally require explicit conversion
   ${asList("Hello", "World") must haveSize(2)}
   ${asList("Hello", "World").asScala must containMatch("ll")}

  """
  // HELPERS
  class Food
  case class Pizza() extends Food
  case class Fruit() extends Food

  case class A(i: Int = 0, j: Int = 1)

}
