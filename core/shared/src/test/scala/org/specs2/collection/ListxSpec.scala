package org.specs2
package collection

import mutable.Spec
import Listx.*

class ListxSpec extends Spec:

  "A safeTranspose function should" >> {
    "transpose columns and rows of a list of lists" >> {
      List(List(1, 2), List(3, 4)).safeTranspose must ===(List(List(1, 3), List(2, 4)))
    }
    "work even if the input is not a matrix" >> {
      List(List(1, 2), List(3, 4, 5)).safeTranspose must ===(List(List(1, 3), List(2, 4), List(5)))
    }
    "work when the input list is empty" >> {
      (Nil: List[List[Int]]).safeTranspose must ===((Nil: List[List[Int]]))
    }
    "work when the input list contains an empty list" >> {
      List(Nil:List[Int], List(1)).safeTranspose must ===(List(List(1)))
    }
  }
