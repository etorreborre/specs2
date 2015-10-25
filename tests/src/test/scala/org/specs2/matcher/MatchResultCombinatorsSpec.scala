package org.specs2
package matcher

import MatchersCreation._
import io.StringOutput
import org.specs2.specification.core.SpecStructure
import specification.AllExpectations

/**
 * all these examples works in a mutable specification which means that FailureExceptions are caught before being
 * combined with the logical combinator
 */
class MatchResultCombinatorsSpec extends mutable.Spec with ResultMatchers with MatchResultCombinators with TypedEqual {

  "Match results can be combined with and" >> {
    (1 must_== 1) and (2 must_== 2)
    1 must be_==(1) and be_==(1)

    ((1 must_== 2) and (2 must_== 2)) must throwA[MatchFailureException[Int]]
    (1 must be_==(2) and be_==(1)) must throwA[MatchFailureException[Int]]
    (1 must be_==(1) and be_==(2)) must throwA[MatchFailureException[Int]]
    (1 must be_>(1) ^^ ((i: Int) => i) and be_==(1)) must throwA[MatchFailureException[Int]]
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

      (1 must beOk and beKo) must throwA[MatchFailureException[_]]
      messages === Seq("ok", "ko")
    }
  }

  "Match results can be combined with or" >> {
    (1 must_== 2) or (2 must_== 2)
    (1 must_== 1) or (1 must_== 2) or (1 must_== 3)
    ((1 must_== 2) or (1 must_== 3) or (1 must_== 4)) must throwA[MatchFailureException[Int]]
    ((1 must_== 2) or (2 must_== 3)) must beFailing
    (1 must_== 2) or success
  }

  "A match result can be negated" >> {
    (1 must_== 2).not
    (1 must_== 1).not must beFailing
    (1 must_== 2).not must beSuccessful
  }

  "A match result can be evaluated only when a boolean condition is satisfied" >> {
    (1 must_== 2).when(false)
    (1 must_== 2).when(true) must throwAn[Exception]
  }

  "A match result can be evaluated only unless a boolean condition is satisfied" >> {
    (1 must_== 2).unless(true)
    (1 must_== 2).unless(false) must throwAn[Exception]
  }

  "A match result can be evaluated if and only if a boolean condition is satisfied" >> {
    (1 must_== 2).iff(false)
    (1 must_== 1).iff(true)
    (1 must_== 2).iff(true) must throwAn[Exception]
    (1 must_== 1).iff(false) must throwAn[Exception]
  }

  "A match result with an or condition where an exception is thrown during the first match" >> {
    val matcher: Matcher[Int] = be_>(0) or throwAn[ArrayIndexOutOfBoundsException] or throwAn[IllegalArgumentException]
    ({ throw new IllegalArgumentException; 1} must matcher) must beSuccessful
  }

  "MatchResult combinators work with the AllExpectations trait" >> {
    val expectations = new Specification with AllExpectations { def is = SpecStructure.empty(getClass)
      val mr = "hello world" must haveSize(10) and startWith("bell")
    }
    val stored = expectations.storedResults

    "there is only one stored expectation (see #320)" ==> { stored must haveSize(1) }
    stored.map(_.message).head must_== "'hello world' doesn't have size 10 but size 11"
  }
}
