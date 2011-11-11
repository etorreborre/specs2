package org.specs2
package execute

import ResultImplicits._

class ResultImplicitsSpec extends Specification { def is =

  "It is possible to test some values against a function returning results"                                   ^
    "with forall, stopping after the first failure"                                                           ! e1^
    "with foreach, collecting all failures"                                                                   ! e2^
    "with atLeastOnce, collecting all failures"                                                               ! e3^
                                                                                                              end

  def e1 = {
    ((i: Int) => check(i must be_<=(2))).forall(Seq(1, 2, 3, 4)).message must_== "In the sequence '1, 2, 3, 4',"+
      " the 3rd element is failing: 3 is greater than 2"
  }
  def e2 = {
    ((i: Int) => check(i must be_<=(2))).foreach(Seq(1, 2, 3, 4)).message must_== "3 is greater than 2; 4 is greater than 2"
  }
  def e3 = {
    ((i: Int) => check(i must be_<=(2))).atLeastOnce(Seq(1, 2, 3, 4))
  }

  /** this function is necessary to check that the type conversion to Result is indeed working and not conflicting with T => MatchResult[S] */
  def check[T <% Result](t: =>T): Result = t
}