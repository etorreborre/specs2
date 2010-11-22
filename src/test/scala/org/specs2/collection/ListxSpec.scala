package org.specs2
package collection
import Listx._

class ListxSpec extends SpecificationWithJUnit { def is = 

  "A removeFirst function should"                                                         ^
    "remove nothing if the list is empty" ! { 
      (Nil: List[String]).removeFirst(_ == "a") must_== Nil
    }                                                                                     ^
    "remove only the first element of a list satisfying the predicate" ! {
      List("a", "b", "c", "b").removeFirst(_ == "b") must_== List("a", "c", "b")
    }                                                                                     ^
                                                                                          p^
  "A safeTranspose function should"                                                       ^
    "transpose columns and rows in a list of lists" ! {
      List(List(1, 2), List(3, 4)).safeTranspose must_== 
      List(List(1, 3), List(2, 4))
    }                                                                                     ^
    "work even if the input is not a matrix" ! {
      List(List(1, 2), List(3, 4, 5)).safeTranspose must_== 
      List(List(1, 3), List(2, 4), List(5))
    }                                                                                     ^
    "work when the input list is empty" ! {
      (Nil: List[List[Int]]).safeTranspose must_== 
      (Nil: List[List[Int]])
    }                                                                                     ^
    "work when the input list contains an empty list" ! {
      List(Nil:List[Int], List(1)).safeTranspose must_== 
      List(List(1))
    }                                                                                     ^
                                                                                          end
}