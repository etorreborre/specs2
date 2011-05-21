package org.specs2
package collection

import mutable.Specification
import Listx._
import matcher.DataTables

class ListxSpec extends Specification with ScalaCheck with DataTables {

  "A removeFirst function" should {
    "remove the first element satisfying a predicate" in {

      "List"            | "Element to remove" | "Result"         |>
      (Nil:List[Int])   ! 2                   ! (Nil:List[Int])  |
      List(2, 3, 4)     ! 2                   ! List(3, 4)       |
      List(1, 2, 2)     ! 2                   ! List(1, 2)       |
      List(1, 2, 3)     ! 2                   ! List(1, 3)       | { (l, a, r) =>
        l.removeFirst(_==a) must_== r
      }

    }
    "this should work for any list and any element" in check { (l: List[Int], a: Int) =>
      val removed = l removeFirst (_ == a)

      val (withoutA, startWithA) = l span (_ != a)
      removed must_== withoutA ++ startWithA.drop(1)
    }
  }

  "A safeTranspose function" should {
    "transpose columns and rows of a list of lists" in {
      List(List(1, 2), List(3, 4)).safeTranspose must_== List(List(1, 3), List(2, 4))
    }
    "work even if the input is not a matrix" in {
      List(List(1, 2), List(3, 4, 5)).safeTranspose must_== List(List(1, 3), List(2, 4), List(5))
    }
    "work when the input list is empty" in {
      (Nil: List[List[Int]]).safeTranspose must_== (Nil: List[List[Int]])
    }
    "work when the input list contains an empty list" in {
      List(Nil:List[Int], List(1)).safeTranspose must_== List(List(1))
    }
  }
}