package org.specs2
package collection

import mutable.Specification
import Listx._

class ListxSpec extends Specification {

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