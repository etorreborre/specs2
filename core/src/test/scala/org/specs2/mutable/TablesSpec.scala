package org.specs2
package mutable

class TablesSpec extends Specification with Tables {
  "When using the Tables trait".txt

  "The first value of a DataTable can be a String followed by a single !" >> {
    "a" | "b" |>
    "1" ! "1" | { (a, b) => a === b }
  }
}
