package org.specs2
package matcher

import mutable.Specification
import execute._
import io.StringOutput

/**
 * all these examples works in a mutable specification which means that FailureExceptions are caught before being
 * combined with the logical combinator
 */
class MatchResultLogicalCombinatorsSpec extends Specification with ResultMatchers {

  "Match results can be combined with and" >> {
    (1 must_== 1) and (2 must_== 2)
    ((1 must_== 2) and (2 must_== 2)) must throwA[MatchFailureException[_]]
    ((1 must_== 2) and (2 must_== 2)) must beFailing
    (1 must_== 1) and success
  }
  "Match results must not be evaluated twice when failing with and" >> {
    "when the first match is failing" >> {
      var evaluated = 0;
      {evaluated += 1; 1 must_== 2} must beFailing
      evaluated === 1
    }
    "when the second match is failing" >> new StringOutput with specification.Scope {
      def printMsg(m: String) = println(m)
      def beKo: Matcher[Int] = (i: Int) => ({printMsg("ko"); false}, "ko")
      def beOk: Matcher[Int] = (i: Int) => ({printMsg("ok"); true}, "ok")

      (1 must beOk and beKo)
      messages === Seq("ok", "ko")
    }
  }
  "Match results can be combined with or" >> {
    (1 must_== 2) or (2 must_== 2)
    ((1 must_== 2) or (2 must_== 3)) must beFailing
    (1 must_== 2) or success
  }
  "A match result can be negated" >> {
    (1 must_== 2).not
    (1 must_== 1).not must beFailing
    (1 must_== 2).not must beSuccessful
  }
  "A match result can be evaluated only when a boolean condition is satisfied" >> {
    ((1 must_== 2): Result).when(false);
    { ((1 must_== 2): Result).when(true) } must throwAn[Exception]
  }
  "A match result can be evaluated only unless a boolean condition is satisfied" >> {
    ((1 must_== 2): Result).unless(true);
    { ((1 must_== 2): Result).unless(false) } must throwAn[Exception]
  }
  "A match result can be evaluated if and only if a boolean condition is satisfied" >> {
    ((1 must_== 2): Result).iff(false)
    ((1 must_== 1): Result).iff(false);
    { ((1 must_== 2): Result).iff(true) } must throwAn[Exception]
  }

}
