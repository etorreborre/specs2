package examples

import org.specs2._
import matcher.DataTables

/**
 *
 */
class DataTableSpec extends Specification with DataTables { def is =

  "adding integers should just work in scala"  ! addition

  def addition =

    "a"   | "b" | "c" |
     2    !  2  !  4  |
     1    !  1  !  2  |> { (a, b, c) =>  a + b must_== c }

}

