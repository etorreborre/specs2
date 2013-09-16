package org.specs2
package matcher

import specification._
import java.util.Arrays._
import scala.collection.parallel.ParSeq
import control.NumberOfTimes
import scala.collection.JavaConversions.collectionAsScalaIterable

class TraversableMatchersSpec extends Specification with ResultMatchers with Tags with Grouped with NumberOfTimes { def is = s2"""

 We can check the elements of a collection by using matchers
   ${ Seq(1, 2, 3) must contain(2) }
   ${ Seq(1, 2, 3) must contain(be_>=(2)) }
   ${ Seq(1, 2, 3) must not contain(be_>=(4)) }
   ${ Seq(1, 2, 3) must not(contain(be_>=(4))) }
   ${ Seq(1, 2, 3) must contain(be_>=(2)).atLeastOnce }
   ${ Seq(1, 2, 3) must contain(be_>=(2)).exactly(2.times)          }
   ${ Seq(1, 2, 3) must contain(be_>=(2)).atMost(2.times)           }
   ${ Seq(1, 2, 3) must contain(be_>=(2)).atLeast(1.times)          }
   ${ Seq(1, 2, 3) must contain(be_>=(2)).between(1.times, 2.times) }
   ${ Seq(1, 2, 3) must contain(be_>=(0)).forall }
   ${ Seq(1, 2, 3) must contain(be_>=(0)).foreach }
   ${ Seq(1, 2) must contain(anyOf(1, 4)) }
   ${ (Seq(1, 2, 3) must not(contain(anyOf(1, 2, 4)))) returns "There are 2 successes\n'1' is contained in '1, 2, 4'\n'2' is contained in '1, 2, 4'\n" }
   ${ Seq("hello", "world") must contain(matching(".*orld")) }
   ${ Seq("hello", "world") must contain((s: String) => s.size > 2) }
   ${ Seq("1", "2", "3") must contain("3") and contain("2":Any) }
   ${ Seq[Food](Pizza(), new Fruit()) must contain(Pizza()) }
   `not contain(1)` must work in a mutable Scope ${g1.e1}
   a string inclusion must work as well  ${ "abc" must contain('b') }


   Failure messages
   ${ (Seq(1, 2, 3) must contain(4)                        ) returns "List(1, 2, 3) does not contain 4"}
   ${ (Seq(1, 2, 3) must contain(be_>=(4))                 ) returns "There are 3 failures\n1 is less than 4\n2 is less than 4\n3 is less than 4\n" }
   ${ (Seq(1, 2, 3) must not contain(be_>=(2))             ) returns "There are 2 successes\n2 is not less than 2\n3 is not less than 2\n" }
   ${ (Seq(1, 2, 3) must contain(be_>=(3)).atLeast(2.times)) returns "There are 2 failures\n1 is less than 3\n2 is less than 3\n" }


 We can compare a collection to another by using matchers

   ${ Seq[Int]() must contain(exactly(Seq[Int]():_*))                                                }
   ${ Seq(1, 2, 3) must contain(exactly(1, 2, 3))                                                    }
   ${ (Seq(1, 2, 3) must contain(exactly(1, 2, 3, 4))) returns "List(1, 2, 3) does not contain 4"    }
   ${ Seq(1, 2, 3) must contain(exactly(be_>=(0), be_>=(1), be_>=(2)))                               }
   ${ Seq(1, 2, 3) must contain(exactly(be_>=(0), be_>=(1), be_>=(2)).inOrder)                       }

   // this must be understood as allOf(2, 3)
   ${ Seq(1, 2, 3) must contain(2, 3) }
   ${ Seq(1, 2, 3) must contain(allOf(Seq(1, 2).map(be_>=(_)):_*))             }
   ${ Seq(1, 2, 3) must contain(allOf(Seq(1, 2).map(be_>=(_)):_*)).inOrder     }
   ${ Seq(1, 2, 3) must not(contain(allOf(Seq(0, 0).map(be_<=(_)):_*)))        }
   ${ Seq(1, 2, 3) must contain(allOf(1, 3).inOrder)                           }

   ${ Seq(1, 2, 3) must contain(atLeast(Seq[Int]():_*))                        }
   ${ Seq(1, 2, 3) must contain(atLeast(3, 1))                                 }
   ${ Seq(1, 2, 3) must contain(atLeast(be_>=(0), be_>=(1), be_>=(2)))         }

   ${ Seq(1, 2)    must contain(atMost(2, 1, 3))                               }
   ${ Seq(1, 2, 3) must contain(atMost(be_>=(0), be_>=(1), be_>=(2)))          }
   ${ Seq(1, 2, 3) must contain(atMost(be_>=(0), be_>=(1), be_>=(2)).inOrder)  }

   Failure messages
   ${ (Seq[Int]() must contain(exactly(1))                                     ) returns "List() does not contain 1" }
   ${ (Seq(1, 2, 3) must contain(exactly(1, 2))                                ) returns "List(1, 2, 3) must not contain 3" }
   ${ (Seq(1, 2, 3) must contain(exactly(be_>=(0), be_>=(1), be_>=(5)))        ) returns "List(1, 2, 3) does not contain exactly 3 correct values\n3 is less than 5 and there are no more available checks for 3\n" }
   ${ (Seq(1, 2, 3) must contain(exactly(be_>=(0), be_>=(2), be_<=(1)).inOrder)) returns "List(1, 2, 3) does not contain exactly 3 correct values in order\n3 is greater than 1\n" }

   ${ (Seq(1, 2, 3) must contain(atLeast(4, 1))                                ) returns "List(1, 2, 3) does not contain 4" }
   ${ (Seq(1, 2, 3) must contain(atLeast(be_>=(0), be_>=(1), be_<=(1)))        ) returns "List(1, 2, 3) does not contain at least 3 correct values\n3 is greater than 1 and there are no more available checks for 3\n" }

   ${ (Seq(1, 2)    must contain(atMost(1, 3))                                 ) returns "List(1, 2) does not contain 3 and must contain 2" }
   ${ (Seq(1, 2)    must contain(atMost(1))                                    ) returns "List(1, 2) must not contain 2" }

   ${ (Seq(1, 2, 3) must contain(exactly(1, 2, 3, 4))                          ) returns "List(1, 2, 3) does not contain 4" }
   ${ (Seq(1, 2, 3) must contain(exactly(1, 4))                                ) returns "List(1, 2, 3) does not contain 4 and must not contain 2, 3" }
   ${ (Seq(1, 2, 3) must contain(exactly(1, 2, 3, 4, 5))                       ) returns "List(1, 2, 3) does not contain 4, 5" }
   ${ (Seq(1, 2, 3) must contain(atLeast(1, 2, 3, 4))                          ) returns "List(1, 2, 3) does not contain 4" }
   ${ (Seq(1, 2, 3) must contain(atLeast(1, 2, 3, 4, 5))                       ) returns "List(1, 2, 3) does not contain 4, 5" }
   ${ (Seq(1, 2, 3) must contain(atMost(1, 2))                                 ) returns "List(1, 2, 3) must not contain 3" }





  we can check if a traversable contains elements following a given pattern
    ${ Seq("Hello", "World") must containMatch("ll") }
    ${ Seq("Hello", "World") must containPattern(".*llo") }


 Size
 ====

 We can check the size of an traversable
    ${ Seq() must beEmpty }
    ${ Seq() must be empty }
    ${ (Seq() must not be empty) must beFailing }
    ${ Seq(1, 2) must haveSize(2) }
    ${ Seq(1, 2) must have size(2) }
    ${ Seq(1, 2) must not have size(1) }
    ${ Seq(1, 2) must haveLength(2) }
    ${ Seq(1, 2) must have length(2) }
    ${ Seq(1, 2) must not have length(1) }

 We can check the size of an Array
    ${ Array(1, 2) must have size(2) }
    ${ (Array(1, 2) must have size(1)).message must_== "Array(1, 2) doesn't have size 1 but size 2" }

 Sorting
 =======

 We can check if a sequence is sorted
    ${ Seq(1, 2, 3) must beSorted }
    ${ Seq(1, 2, 3) must be sorted }
    ${ Seq(2, 1, 3) must not beSorted }
    // this doesn't compile because of 'diverging implicit'
    //{ Seq(2, 1, 3) must not be sorted }
    ${ (Seq(2, 1, 3) must beSorted) returns "List(2, 1, 3) is not sorted" }

 Compare to another traversable
 ==============================

  We can check if 2 traversables are contained in each other
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

 With Java collections
 =====================

 Java collections can also be used with Traversable matchers but generally require explicit conversion
   ${ asList("Hello", "World") must haveSize(2) }
   ${ collectionAsScalaIterable(asList("Hello", "World")) must containMatch("ll") }

 With Parallel collections
 =========================

 Parallel collections work with any matcher
   ${ ParSeq(1, 2, 3) must contain(allOf(1, 2, 3)) }
                                                                                                                        """

  /**
   * Examples
   */

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

  class Food
  case class Pizza() extends Food
  case class Fruit() extends Food

}
