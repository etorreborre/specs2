package org.specs2
package mutable

import matcher.TypedEqualExpectations

class TablesSpec extends Spec with Tables with TypedEqualExpectations {
  addText("When using the Tables trait")

  "The first value of a DataTable can be a String followed by a single !" >> {
    "a" | "b" |>
    "1" ! "1" | { (a, b) => a === b }
  }
}
