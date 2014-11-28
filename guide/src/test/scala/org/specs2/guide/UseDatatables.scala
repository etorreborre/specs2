package org.specs2
package guide

import specification.Tables

object UseDatatables extends UserGuidePage { def is = "Datatables".title ^ s2"""

DataTables are used to pack several expectations inside one example using a tabular format: ${snippet{

class DataTableSpec extends Specification with org.specs2.specification.Tables { def is = s2"""

 adding integers should just work in scala ${
  // the header of the table, with `|` separated strings (`>` executes the table)
  "a"   | "b" | "c" |>
   2    !  2  !  4  |                   // an example row
   1    !  1  !  2  |                   // another example row
  { (a, b, c) => a + b must_== c }      // the expectation to check on each row
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

### Implicit `!`

The datatable DSL uses the `!` operator to define columns. However this operator is also used by the examples DSL to create the body of an example: `"my example is" ! ok`, so if the first column of the datatable contains strings you will not be able to use both at the same time to mean different things ("create a table header" and "create an example").

You can solve this conflict by either:

 - using the `org.specs2.specification.Tables` and `org.specs2.mutable.Tables` traits which will deactivate the example DSL on acceptance and mutable specifications
 - using the `org.specs2.matcher.DataTables` trait and use `!!` instead of `!` if the first column is a string (for good visual balance you can use `||` in the header)
"""
}
