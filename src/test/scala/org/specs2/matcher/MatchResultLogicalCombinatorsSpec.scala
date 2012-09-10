package org.specs2
package matcher

import mutable.Specification
import execute.ResultExecution

/**
 * all these examples works in a mutable specification which means that FailureExceptions are caught before being
 * combined with the logical combinator
 */
class MatchResultLogicalCombinatorsSpec extends Specification with ResultMatchers {

  "Match results can be combined with and" >> {
    (1 must_== 1) and (2 must_== 2)
    ((1 must_== 2) and (2 must_== 2)) must beFailing
  }
  "Match results can be combined with or" >> {
    (1 must_== 2) or (2 must_== 2)
    ((1 must_== 2) or (2 must_== 3)) must beFailing
  }
  "A match result can be negated" >> {
    (1 must_== 2).not
    (1 must_== 1).not must beFailing
    (1 must_== 2).not must beSuccessful
  }

}
