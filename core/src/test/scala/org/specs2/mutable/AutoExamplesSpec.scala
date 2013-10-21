package org.specs2
package mutable

import matcher.DataTables

class AutoExamplesSpec extends Specification with DataTables {

  "The trimCode function" should {
    "remove formatting fragments" >> {
      "code"                     || "result"                  |>
      "success.p"                !! "success"                 |
      "success p"                !! "success"                 |
      "success.end"              !! "success"                 |
      "success.endp"             !! "success"                 |
      "success.br"               !! "success"                 |
      "success.bt"               !! "success"                 |
      "success.bt"               !! "success"                 |
      "success.t(2)"             !! "success"                 |
      "success.bt(-3)"           !! "success"                 | { (code, result) => trimCode(code) must_== result }
    }
    "remove backticks" >> {
      trimCode("`method`") must_== "method"
    }
  }
}
