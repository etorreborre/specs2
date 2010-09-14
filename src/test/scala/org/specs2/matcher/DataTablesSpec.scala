package org.specs2
package matcher
import execute._

class DataTablesSpec extends Specification with DataTables {
  val examples = 
"""
Data tables
"""^
"simple table" ! e1

import scala.Function._

  def e1 = {
	"a"   | "b" | "c" |
	 1    !  2  !  3  |
	 1    !  2  !  3  |> { (a, b, c) => 
      a + b must_== c
    }
  }
}