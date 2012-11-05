package org.specs2
package execute

import ResultImplicits._
import specification.Grouped

class ResultImplicitsSpec extends Specification with Grouped { def is =

  "It is possible to test some values against a function returning results"                                   ^
    "with forall, stopping after the first failure"                                                           ! g1.e1^
    "with foreach, collecting all failures"                                                                   ! g1.e2^
    "with atLeastOnce, collecting all failures"                                                               ! g1.e3^
                                                                                                              end

  "Collections of results" - new g1 {
    e1 := ((i: Int) => i must be_<=(2)).forall(Seq(1, 2, 3, 4)).message ===
             "In the sequence '1, 2, 3, 4', the 3rd element is failing: 3 is greater than 2"

    e2 := ((i: Int) => i must be_<=(2)).foreach(Seq(1, 2, 3, 4)).message ===
             "3 is greater than 2; 4 is greater than 2"

    e3 := ((i: Int) => i must be_<=(2)).atLeastOnce(Seq(1, 2, 3, 4))
  }

}