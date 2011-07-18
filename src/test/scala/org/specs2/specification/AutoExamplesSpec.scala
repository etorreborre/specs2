package org.specs2
package specification

import AutoExamples._
import matcher.DataTables

class AutoExamplesSpec extends Specification with DataTables { def is =

  "The trimCode function should"                                                    ^
    "remove formatting fragments"                                                   ! e1^
    "remove backticks"                                                              ^
      "with no parameter list"                                                      ! e2^
      "with a parameter list - one param"                                           ! e3^
      "with a parameter list - 2 params"                                            ! e4^
                                                                                    end

  def e1 = "code"                     || "result"                  |>
           "success ^"                !! "success"                 |
           "success ^end^"            !! "success"                 |
           "success ^ end"            !! "success"                 |
           "{ success } ^ end"        !! "success"                 | { (code, result) => trimCode(code) must_== result }

  def e2 = trimCode("`method`") must_== "method"

  def e3 = trimCode("`method`(p1)") must_== "method"

  def e4 = trimCode("`method`(p1, p2)") must_== "method"
}