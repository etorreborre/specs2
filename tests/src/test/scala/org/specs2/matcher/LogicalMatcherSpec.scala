package org.specs2
package matcher

import execute._
import specification._
import sys._
import io.StringOutput
import MatchResultCombinators._

class LogicalMatcherSpec extends script.Spec with ResultMatchers with Grouped with StringMatchers with TraversableMatchers with TypedEqual { def is = s2"""

Or matches
==========

 A matcher can be or-ed with another one
   + if both matches are ok the result is ok
   + if both matches are ko the result is ko
   + if the first matcher is ok, the second one is not evaluated
   + if both matchers are ko the combination is ko
   + if the first matcher is ko, and the second ok, the combination is ok
   + if the matchers throw exceptions the combination must be a success when 'failure' or 'success'

 it is possible also to 'or' 2 match expressions
   + if the first is ok, the result is ok
   + if the first is not ok, the second is not evaluated
   + ko and ok and ko is ok

 `or` must deal with exceptions
   + { throw e; 1 } must m1 or throwAn[Exception]
   + { throw e; 1 } must throwAn[Exception] or m1
   + { 1          } must m1 or throwAn[Exception]
   + { 1          } must throwAn[Exception] or m1
   + { throw E2; 1 } must m1 or throwAn[E2] or throwAn[E1]

And matches
===========

 a matcher can be and-ed with another one
   + if both matches are ok the result is ok

 it is possible also to 'and' 2 match expressions
   + if both matches are ok the result is ok
   + if the first is not ok, the second is not evaluated
   + ok and ko and ok is ko

Succeed or skip, or pending
===========================

 a matcher can be ok or be skipped
   + if it is ok, it returns a MatchSuccess result
   + if it is ko, it returns a MatchSkip result
   + if it throws an exception, it returns a MatchSkip result
   + a skipped message can also be added in front of the failure message

 a matcher can be ok or be pending
   + if it is ok, it returns a MatchSuccess result
   + if it is ko, it returns a MatchPending result
   + if it throws an exception, it returns a MatchPending result
   + a pending message can also be added in front of the failure message

Conditions
==========

 a matcher can applied only if a boolean condition is true
   + if the condition is true, it is applied
   + if the condition is false, it is not and a success is returned
   + if the condition is false, a message can be added to the success result
   + 'unless' can also be used to avoid negating the condition

 a matcher can applied if and only if a boolean condition is true
   if the condition is true, it is applied
     + the result is true if the application is true
     + the result is false if the application is false

   if the condition is false, it is applied
     + the result is true if the application is false
     + the result is false if the application is true

Custom
======

 + a custom match can be negated, or used with be/have
                                                                                                                """

  "or matches" - new group {
    eg := "eric" must (beMatching("e.*") or beMatching(".*c"))
    eg := "eric" must (beMatching("a.*") or beMatching(".*z")).not
    eg := "eric" must (beMatching("e.*") or beMatching({error("boom");".*z"}))
    eg := "eric" must not (beMatching("a.*") or beMatching(".*z"))
    eg := ("eric" must (beMatching("a.*") or beMatching("z.*"))) returns
            "'eric' doesn't match 'a.*'; 'eric' doesn't match 'z.*'"

    eg := new Scope with MustThrownMatchers { "eric" must (beMatching("a.*") or beMatching("e.*"))  }

    eg := ("eric" must be matching("e.*")) or ("eric" must be matching(".*d"))
    eg := {
      val out = new StringOutput {}
      ("eric" must be matching("e.*")) or { out.println("DONT"); "torreborre" must be matching(".*tor.*") }
      out.messages must not contain("DONT")
    }
    eg := ((true === false) or (true === true) or (true === false)) must beSuccessful

    eg := { throw new Exception("ouch"); 1 } must be_==(1) or throwAn[Exception]
    eg := { throw new Exception("ouch"); 1 } must throwAn[Exception] or be_==(1)
    eg := {                              1 } must throwAn[Exception] or be_==(1)
    eg := {                              1 } must be_==(1) or throwAn[Exception]
    eg := { throw new Exception("ouch"); 1 } must be_==(1) or throwAn[Exception] or throwAn[Exception]
  }


  "and" - new group {
    eg := "eric" must be matching("e.*") and be matching(".*c")

    eg := ("eric" must be matching("e.*")) and ("torreborre" must be matching(".*tor.*"))
    eg := {
      val out = new StringOutput {}
      ("eric" must be matching("x.*")) and { out.println("DONT"); "torreborre" must be matching(".*tor.*") }
      out.messages must not contain("DONT")
    }
    eg := ((true === true) and (true === false) and (true === true)) must beFailing
  }

  "orSkip, orPending" - new group {
    eg := 1 must be_==(1).orSkip
    eg := (1 must be_==(2).orSkip).toResult                                  must_== Skipped("1 != 2")
    eg := (1 must be_==({sys.error("boom");2}).orSkip("skip this")).toResult must_== Skipped("skip this: boom")
    eg := (1 must be_==(2).orSkip("precondition failed")).toResult           must_== Skipped("precondition failed: 1 != 2")

    eg := 1 must be_==(1).orPending
    eg := (1 must be_==(2).orPending).toResult                             must_== Pending("1 != 2")
    eg := (1 must be_==({sys.error("boom");2}).orPending("todo")).toResult must_== Pending("todo: boom")
    eg := (1 must be_==(2).orPending("precondition failed")).toResult      must_== Pending("precondition failed: 1 != 2")
  }

  "condition" - new group {
    eg := (1 must be_==(1).when(true)).toResult               must beSuccessful
    eg := (1 must be_==(2).when(false)).toResult              must beSuccessful
    eg := (1 must be_==(2).when(false, "no worries")).message must_== "no worries"
    eg := (1 must be_==(2).unless(true)).toResult             must beSuccessful

    eg := (1 must be_==(1).iff(true)).toResult  must beSuccessful
    eg := (1 must be_==(2).iff(true)).toResult  must beFailing

    eg := (1 must be_==(2).iff(false)).toResult must beSuccessful
    eg := (1 must be_==(1).iff(false)).toResult must beFailing
  }

  "with custom matchers" - new group {
    eg := (12 must bePositive) and
          (12 must be positive) and
          (-12 must not bePositive) and
          (-12 must not be positive)
  }

  case class CustomMatcher[T : Numeric]() extends Matcher[T] {
    def apply[S <: T](e: Expectable[S]) =
      result(implicitly[Numeric[T]].abs(e.value) == e.value, e.value+" is positive", e.value+" is negative", e)
  }
  /** this allows to write "a must not bePositive" or "a must be positive" */
  lazy val outer = this
  implicit def anyBePositive[T : Numeric](result: MatchResult[T]) = new AnyBePositive(result)
  class AnyBePositive[T : Numeric](result: MatchResult[T]) {
    def bePositive = result(outer.bePositive)
    def positive = result(outer.bePositive)
  }

  /** custom matcher */
  def bePositive[T : Numeric] = CustomMatcher[T]()
  /** this allows to write "a must not be positive" */
  def positive[T : Numeric] = bePositive

}