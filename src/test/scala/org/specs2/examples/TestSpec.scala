package org.specs2
package examples

import matcher.DataTables

class TestSpec extends Specification with DataTables { def is =
  "text" ^
  (tOk)

  val tOk    = "a" | "b" |> 1 ! 1 | { (a, b) => a must_== b }
  val tKo    = "a" | "b" |> 1 ! 2 | { (a, b) => a must_== b }

}
