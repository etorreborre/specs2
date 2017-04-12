package examples

import org.specs2._
import matcher.DataTables

/**
 * This specification shows how to use a simple Datatable
 */
class DataTablesSpec extends Specification with DataTables { def is = s2"""

 Adding integers should just work in scala $addition
                                                                        """
  def addition =

    "a"   | "b" | "c" |>
     2    !  2  !  4  |
     1    !  1  !  2  | { (a, b, c) =>  a + b === c }

}

