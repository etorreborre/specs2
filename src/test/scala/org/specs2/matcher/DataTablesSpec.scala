package org.specs2
package matcher
import execute._

class DataTablesSpec extends Specification with DataTables {
	override def args = "stacktrace"
  val examples = 
"""
Data tables
"""^
"simple table" ! e1

  def e1 = {
	"a"   | "b" | "c" |
	 2    !  2  !  4  |
	 1    !  1  !  2  |> { (a, b, c) => 
      a + b must_== c
    }
  }
}