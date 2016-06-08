package org.specs2
package guide

import org.specs2.concurrent.ExecutionEnv
import specification.Tables
import execute._
import scala.concurrent.Future
import matcher.DataTable
import scalaz._, Scalaz._

object UseDatatables extends UserGuidePage with Tables { def is = "Datatables".title ^ s2"""

DataTables are used to pack several expectations inside one example using a tabular format: ${snippet{

class DataTableSpec extends Specification with org.specs2.specification.Tables { def is = s2"""

 adding integers should just work in scala ${
// the header of the table, with `|` separated strings (`>` executes the table)
"a" | "b" | "c" |>
2   !  2  !  4  |                   // an example row
1   !  1  !  2  |                   // another example row
  { (a, b, c) => a + b must_== c }  // the expectation to check on each row
}
"""
}
}}

A `DataTable` which is used as a `Result` in the body of an Example will only be displayed when failing. If you also want to display the table when successful, to document your examples, you can omit the example description and inline the DataTable directly in the specification:${snippet{
class DataTableSpec extends Specification with Tables { def is = s2"""

 adding integers should just work in scala
  ${
     "a"   | "b" | "c" |>
      2    !  2  !  4  |
      1    !  1  !  2  |
      { (a, b, c) => a + b must_== c }
   }
"""
  }
}}

This specification will be rendered as:
```
adding integers should just work in scala
+  a | b | c |
   2 | 2 | 4 |
   1 | 1 | 2 |
```

### Format columns

The display of elements can be modified by using an implicit `org.specs2.text.Showx` instance where `x` corresponds to
the number of columns in the table. For example: ${snippet{
import org.specs2.text._

implicit val s3 =
  Show3[Int, Double, String]().copy(show2 = (d: Double) => "x" * d.toInt)

val table =
"a" | "b"   | "c" |>
 1  ! 2.0   ! "3" |
 2  ! 4.0   ! "6" | { (a: Int, b: Double, c: String) => (a + b.toInt).toString ==== c}

"table result\n"+table.message
}.eval}

### Implicit `!`

The datatable DSL uses the `!` operator to define columns. However this operator is also used by the examples DSL to create the body of an example: `"my example is" ! ok`, so if the first column of the datatable contains strings you will not be able to use both at the same time to mean different things ("create a table header" and "create an example").

You can solve this conflict by either:

 - using the `org.specs2.specification.Tables` and `org.specs2.mutable.Tables` traits which will deactivate the example DSL on acceptance and mutable specifications
 - using the `org.specs2.matcher.DataTables` trait and use `!!` instead of `!` if the first column is a string (for good visual balance you can use `||` in the header)

### Concurrent execution

By default the execution of a datatable is sequential, one row after another. This might not be very practical if you have long-running computations on each row.
If this is the case you can use the `|*` operator (instead of just `|`) to define your execution function:${snippet{
  "a"   | "b" | "c" |>
   2    !  2  !  4  |
   1    !  1  !  2  |* { (a, b, c) => a + b must_== c }
}}

This returns a function `ExecutorService => Result` which can be used directly as the body of an example. You can also pass it your own thread pool by creating, for example, `java.util.concurrent.Executors.newFixedThreadPool(4)`.

More generally, you can use the "Applicative" operator `|@` to pass anything having a `scalaz.Applicative` instance, like a `scala.concurrent.Future`:${snippet {
  // this table uses the global execution context implicitly to create futures
  // scala.concurrent.ExecutionContext.Implicits.global
  def result: scala.concurrent.Future[DecoratedResult[DataTable]] =
    "a" | "b" | "c" |>
     2  !  2  ! 4   |
     1  !  1  ! 2   |@ { (a, b, c) => Future(a + b must_== c) }

  // then you need to get an implicit execution environment and
  // await on the Future result
  implicit def ee: ExecutionEnv = ???
  result.await
}}


"""
}
