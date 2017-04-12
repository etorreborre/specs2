package org.specs2
package matcher

import text._

class DataTableShowSpec extends Specification with DataTables { def is = s2"""

 Implicit Show definitions can be used to adjust the display of data tables $display

 A show instance can specify how to format one column only $oneOnly

"""

  def display = {
    implicit val s3 = Show3[Int, Double, String](
      (i: Int) => "x"*i,
      (d: Double) => "y"*d.toInt,
      (s: String) => s"($s)"
    )

    val table =
      "a" | "b" | "c"     |>
       1  ! 2.0 ! "three" |
       1  ! 2.0 ! "three" | { (a: Int, b: Double, c: String) => ok }

    table.message ====
      List("  | a | b  | c       | ",
           "+ | x | yy | (three) | ",
           "+ | x | yy | (three) | ").mkString("\n")
  }

  def oneOnly = {
    implicit val s3 =
      Show3[Int, Double, String]().copy(show2 = (d: Double) => "y"*d.toInt)

    val table =
      "a" | "b" | "c"     |>
       1  ! 2.0 ! "three" |
       1  ! 2.0 ! "three" | { (a: Int, b: Double, c: String) => ok }

    table.message ====
      List("  | a | b  | c     | ",
           "+ | 1 | yy | three | ",
           "+ | 1 | yy | three | ").mkString("\n")


  }

}
