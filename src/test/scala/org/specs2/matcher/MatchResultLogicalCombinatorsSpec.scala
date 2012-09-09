package org.specs2
package matcher

import mutable.Specification

class MatchResultLogicalCombinatorsSpec extends Specification {
  "Match results can be combined with and" >> {
    (1 must_== 1) and (2 must_== 2)
  }
  "Match results can be combined with or" >> {
    (1 must_== 2) or (2 must_== 2)
  }
  "A match result can be negated" >> {
    (1 must_== 2).not
  }
}
