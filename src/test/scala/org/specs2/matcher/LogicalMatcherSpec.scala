package org.specs2
package matcher
import execute._
import specification._
import sys._
import io.MockOutput

class LogicalMatcherSpec extends Specification with ResultMatchers with Grouped { def is = //args.select(ex="throwAn") ^
  "a matcher can be or-ed with another one"                                                                             ^
    "if both matches are ok the result is ok"                                                                           ! g1.e1^
    "if both matches are ko the result is ko"                                                                           ! g1.e2^
    "if the first matcher is ok, the second one is not evaluated"                                                       ! g1.e3^
    "if both matchers are ko the combination is ko"                                                                     ! g1.e4^
    "if the first matcher is ko, and the second ok, the combination is ok"                                              ! g1.e5^
    "if the matchers throw exceptions the combination must be a success when 'failure' or 'success'"                    ! g1.e6^
                                                                                                                        p^
  "it is possible also to 'or' 2 match expressions"                                                                     ^
    "if the first is ok, the result is ok"                                                                              ! g1.e7^
    "if the first is not ok, the second is not evaluated"                                                               ! g1.e8^
    "ko and ok and ko is ok"                                                                                            ! g1.e9^
                                                                                                                        p^
  "a matcher can be and-ed with another one"                                                                            ^
    "if both matches are ok the result is ok"                                                                           ! g2.e1^
                                                                                                                        p^
  "it is possible also to 'and' 2 match expressions"                                                                    ^
    "if both matches are ok the result is ok"                                                                           ! g2.e2^
    "if the first is not ok, the second is not evaluated"                                                               ! g2.e3^
    "ok and ko and ok is ko"                                                                                            ! g2.e4^
                                                                                                                        p^
  "a matcher can be ok or be skipped"                                                                                   ^
    "if it is ok, it returns a MatchSuccess result"                                                                     ! g3.e1^
    "if it is ko, it returns a MatchSkip result"                                                                        ! g3.e2^
    "if it throws an exception, it returns a MatchSkip result"                                                          ! g3.e3^
    "a skipped message can also be added in front of the failure message"                                               ! g3.e4^
                                                                                                                        p^
    "a matcher can be ok or be pending"                                                                                 ^
    "if it is ok, it returns a MatchSuccess result"                                                                     ! g4.e1^
    "if it is ko, it returns a MatchPending result"                                                                     ! g4.e2^
    "if it throws an exception, it returns a MatchPending result"                                                       ! g4.e3^
    "a pending message can also be added in front of the failure message"                                               ! g4.e4^
                                                                                                                        p^
  "a matcher can applied only if a boolean condition is true"                                                           ^
    "if the condition is true, it is applied"                                                                           ! g5.e1^
    "if the condition is false, it is not and a success is returned"                                                    ! g5.e2^
    "if the condition is false, a message can be added to the success result"                                           ! g5.e3^
    "'unless' can also be used to avoid negating the condition"                                                         ! g5.e4^
                                                                                                                        p^
  "a matcher can applied if and only if a boolean condition is true"                                                    ^
    "if the condition is true, it is applied"                                                                           ^
      "the result is true if the application is true"                                                                   ! g6.e1^
      "the result is false if the application is false"                                                                 ! g6.e2^
                                                                                                                        p^
    "if the condition is false, it is applied"                                                                          ^
      "the result is true if the application is false"                                                                  ! g6.e3^
      "the result is false if the application is true"                                                                  ! g6.e4^
                                                                                                                        p^
  "a customer matcher can be negated, or used with be/have"                                                             ! g7.e1^
  "with exceptions"                                                                                                     ^
    "{ throw e; 1 } must m1 or throwAn[Exception]"                                                                      ! g8.e1^
    "{ throw e; 1 } must throwAn[Exception] or m1"                                                                      ! g8.e2^
    "{ 1          } must m1 or throwAn[Exception]"                                                                      ! g8.e3^
    "{ 1          } must throwAn[Exception] or m1"                                                                      ! g8.e4^
    "{ throw E2; 1 } must m1 or throwAn[E2] or throwAn[E1]"                                                             ! g8.e5^
    end

  "or" - new g1 {
    e1 := "eric" must (beMatching("e.*") or beMatching(".*c"))
    e2 := "eric" must (beMatching("a.*") or beMatching(".*z")).not
    e3 := "eric" must (beMatching("e.*") or beMatching({error("boom");".*z"}))
    e4 := "eric" must not (beMatching("a.*") or beMatching(".*z"))
    e5 := ("eric" must (beMatching("a.*") or beMatching("z.*"))) returns
            "'eric' doesn't match 'a.*'; 'eric' doesn't match 'z.*'"

    e6 := new Scope with MustThrownMatchers { "eric" must (beMatching("a.*") or beMatching("e.*"))  }
    e7 := ("eric" must be matching("e.*")) or ("eric" must be matching(".*d"))
    e8 := {
      val out = new MockOutput {}
      ("eric" must be matching("e.*")) or { out.println("DONT"); "torreborre" must be matching(".*tor.*") }
      out.messages must not contain("DONT")
    }
    e9 := ((true === false) or (true === true) or (true === false)) must beSuccessful
  }

  "and" - new g2 {
    e1 := "eric" must be matching("e.*") and be matching(".*c")
    e2 := ("eric" must be matching("e.*")) and ("torreborre" must be matching(".*tor.*"))
    e3 := {
      val out = new MockOutput {}
      ("eric" must be matching("x.*")) and { out.println("DONT"); "torreborre" must be matching(".*tor.*") }
      out.messages must not contain("DONT")
    }
    e4 := ((true === true) and (true === false) and (true === true)) must beFailing
  }

  "skip" - new g3 {
    e1 := 1 must be_==(1).orSkip
    e2 := (1 must be_==(2).orSkip).toResult                                  must_== Skipped("'1' is not equal to '2'")
    e3 := (1 must be_==({sys.error("boom");2}).orSkip("skip this")).toResult must_== Skipped("skip this: boom")
    e4 := (1 must be_==(2).orSkip("precondition failed")).toResult           must_== Skipped("precondition failed: '1' is not equal to '2'")
  }

  "pending" - new g4 {
    e1 := 1 must be_==(1).orPending
    e2 := (1 must be_==(2).orPending).toResult                             must_== Pending("'1' is not equal to '2'")
    e3 := (1 must be_==({sys.error("boom");2}).orPending("todo")).toResult must_== Pending("todo: boom")
    e4 := (1 must be_==(2).orPending("precondition failed")).toResult      must_== Pending("precondition failed: '1' is not equal to '2'")
  }

  "when" - new g5 {
    e1 := (1 must be_==(1).when(true)).toResult               must beSuccessful
    e2 := (1 must be_==(2).when(false)).toResult              must beSuccessful
    e3 := (1 must be_==(2).when(false, "no worries")).message must_== "no worries"
    e4 := (1 must be_==(2).unless(true)).toResult             must beSuccessful
  }

  "if and only if" - new g6 {
    e1 := (1 must be_==(1).iff(true)).toResult  must beSuccessful
    e2 := (1 must be_==(2).iff(true)).toResult  must beFailing
    e3 := (1 must be_==(2).iff(false)).toResult must beSuccessful
    e4 := (1 must be_==(1).iff(false)).toResult must beFailing
  }

  "with custom matchers" - new g7 {
    /** custom matcher */
    def bePositive[T : Numeric] = CustomMatcher[T]()
    /** this allows to write "a must not be positive" */
    def positive[T : Numeric] = bePositive

    case class CustomMatcher[T : Numeric]() extends Matcher[T] {
      def apply[S <: T](e: Expectable[S]) =
        result(implicitly[Numeric[T]].abs(e.value) == e.value, e.value+" is positive", e.value+" is negative", e)
    }
    /** this allows to write "a must not bePositive" or "a must be positive" */
    val outer = this
    implicit def anyBePositive[T : Numeric](result: MatchResult[T]) = new AnyBePositive(result)
    class AnyBePositive[T : Numeric](result: MatchResult[T]) {
      def bePositive = result(outer.bePositive)
      def positive = result(outer.bePositive)
    }
    e1 := (12 must bePositive) and
          (12 must be positive)
          (-12 must not bePositive) and
          (-12 must not be positive)
  }

  "with exceptions" - new g8 {
    e1 := { throw new Exception("ouch"); 1 } must be_==(1) or throwAn[Exception]
    e2 := { throw new Exception("ouch"); 1 } must throwAn[Exception] or be_==(1)
    e3 := {                              1 } must throwAn[Exception] or be_==(1)
    e4 := {                              1 } must be_==(1) or throwAn[Exception]
    e5 := { throw new Exception("ouch"); 1 } must be_==(1) or throwAn[Exception] or throwAn[Exception]
  }
}