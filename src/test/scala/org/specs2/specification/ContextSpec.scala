package org.specs2
package specification

import io._
import sys._
import execute._
import matcher._
import _root_.org.specs2.mutable.{Around => MAround, Before => MBefore, After => MAfter, Specification => Spec}

class ContextSpec extends Specification with FragmentExecution with ResultMatchers { def is =
                                                                                                                        """
  It is sometimes necessary to provide functions to "prepare" the specification before executing the Fragments
  and clean it up afterwards. This may be for example:

     * opening a database connection
     * inserting some data
     * executing the example
     * closing the connection after each example
     
  It may also be very convenient to have each example executed "inside" a specific context, like a  web application
  session. Finally, some setups or cleanups are very expensive so one might want to add arbitrary actions that will
  be executed only once, at the beginning of the specification or the end.
  
  All of this can be achieved in specs2 by using case classes which extend the following traits:

     * Before
     * After
     * Around
     * Outside
     * BeforeAfter or BeforeAfterAround or AroundOutside for combined functionality
                                                                                                                        """^
  "The Before trait can be used to execute methods before Fragments"                                                    ^
    "the before method is executed before a first example"                                                              ! before().e1^
    "the before method is executed before the second example"                                                           ! before().e2^
                                                                                                                        p^
  "If the before method throws an exception"                                                                            ^
    "the first example will not execute"                                                                                ! before().e3^
    "and it will be reported as an error"                                                                               ! before().e4^
                                                                                                                        p^
  "If the before method returns Skipped"                                                                                ^
    "the first example will not execute"                                                                                ! before().e5^
    "and it will be reported as skipped with the reason"                                                                ! before().e6^
                                                                                                                        p^
  "If the before method throws a SkippedException"                                                                      ^
    "the first example will not execute"                                                                                ! before().e5_1^
    "and it will be reported as skipped with the reason"                                                                ! before().e6_1^
                                                                                                                        p^
  "If the before method returns a MatchFailure"                                                                         ^
    "the first example will not execute"                                                                                ! before().e7^
    "and it will be reported as failed with the reason"                                                                 ! before().e8^
                                                                                                                        p^
  "If the before method returns a MatchFailure"                                                                         ^
    "the first example will not execute"                                                                                ! before().e7_1^
    "and it will be reported as failed with the reason"                                                                 ! before().e8_1^
                                                                                                                        p^
  "The After trait can be used to execute methods after Fragments"                                                      ^
    "the after method is executed after a first example"                                                                ! c().e5^
    "the after method is executed after the second example"                                                             ! c().e6^
    "if the expectation fails then the example must fail as well"                                                       ! c().e6_1^
                                                                                                                        p^
  "If the after method throws an exception"                                                                             ^
    "the first example will execute"                                                                                    ! c().e7^
    "but it will be reported as an error"                                                                               ! c().e8^
                                                                                                                        p^
  "Any result can be wrapped in an After context with an implicit"                                                      ! c().e8_1^
                                                                                                                        p^
  "The Around trait can be used to"                                                                                     ^
    "execute the example inside a user provided context"                                                                ! c().e9^
                                                                                                                        p^
  "The Outside trait can be used to"                                                                                    ^
    "execute the example inside a user provided function"                                                               ! c().e9_1^
    "get a proper error if the context preparation fails"                                                               ! c().e9_2^
                                                                                                                        p^
  "The AroundOutside trait can be used to"                                                                              ^
    "execute the example inside a user provided function, with some code around"                                        ! c().e9_3^
                                                                                                                        p^
  "The BeforeAfter trait can be used to"                                                                                ^
    "execute a method before and after each example"                                                                    ! c().e10^
                                                                                                                        p^
  "The BeforeAfterAround trait can be used to"                                                                          ^
    "execute a method before, around and after the first example"                                                       ! c().e11^
                                                                                                                        p^
  "The Before After Around traits can be composed to create more complex setups"                                        ^
    "before compose before2 "                                                                                           ! compose().e1^
    "before then before2 "                                                                                              ! compose().e2^
    "after compose after2 "                                                                                             ! compose().e3^
    "after then after2 "                                                                                                ! compose().e4^
    "beforeAfter compose before2After2 "                                                                                ! compose().e5^
    "beforeAfter then before2After2 "                                                                                   ! compose().e6^
    "around compose around2 "                                                                                           ! compose().e7^
    "around then around2 "                                                                                              ! compose().e8^
    "beforeAfterAround compose before2After2Around"                                                                     ! compose().e9^
    "beforeAfterAround then before2After2Around2"                                                                       ! compose().e10^
                                                                                                                        p^
  "An Action can be used to create Step fragments containing an action to execute:"                                     ^
    "val beforeSpec = new Action"                                                                                       ^
    "def is = beforeSpec(c.println('beforeSpec')) ^ ex1"                                                                ^
                                                                                                                        p^
    "that action will execute and return a result"                                                                      ! c().e12^
    "if it executes ok, nothing is printed, it is a silent Success"                                                     ! c().e13^
    "otherwise, it is reported as an Error"                                                                             ! c().e14^
                                                                                                                        p^
  "A context can be passed applied to many fragments"                                                                   ^
    "for an acceptance spec"                                                                                            ! applyEach().e1^
    "for a mutable spec"                                                                                                ! applyEach().e2^
    "for an outside context and an acceptance spec"                                                                     ! applyEach().e3^
    "for an outside context and a mutable spec"                                                                         ! applyEach().e4^
                                                                                                                        p^
  "In a mutable spec"                                                                                                   ^
    "the before code must be called before the body code"                                                               ! mutableSpec().before1^
    "the after code must be called after the body code"                                                                 ! mutableSpec().after1^
    "the around code must be called around the body code"                                                               ! mutableSpec().around1^
                                                                                                                        end
  implicit val arguments = main.Arguments()
  case class before() extends FragmentsExecution {
    def e1 = executing(ex1Before).prints("before", "e1")
    def e2 = executing(ex1_2Before).prints("before", "e1", "before", "e2")
    def e3 = executing(ex1_beforeFail).prints()
    def e4 = executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
    def e5 = executing(ex1_beforeSkipped).prints()
    def e6 = executeBodies(ex1_beforeSkipped).map(_.message) must_== List("skipped")
    def e5_1 = executing(ex1_beforeSkippedThrown).prints()
    def e6_1 = executeBodies(ex1_beforeSkippedThrown).map(_.message) must_== List("skipped")
    def e7 = executing(ex1_beforeMatchFailed).prints()
    def e8 = executeBodies(ex1_beforeMatchFailed).map(_.message) must_== List("'1' is not equal to '2'")
    def e7_1 = executing(ex1_beforeMatchFailedThrown).prints()
    def e8_1 = executeBodies(ex1_beforeMatchFailedThrown).map(_.message) must_== List("'1' is not equal to '2'")
  }
  case class c() extends FragmentsExecution {
    def e5 = executing(ex1After).prints("e1", "after")
    def e6 = executing(ex1_2After).prints("e1", "after", "e2", "after")
    def e6_1 = executeBodies(ex1FailAfter).head must beFailing
    def e7 = executing(ex1_afterFail).prints("e1")
    def e8 = executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
    def e8_1 = executing(ex1ImplicitAfter).prints("e1", "after")
    def e9 = executing(ex1Around).prints("around", "e1")
    def e9_1 = executing(ex1Outside).prints("outside", "e1")
    def e9_2 = executeBodies(ex1OutsideFail).map(_.message) must_== List("error")
    def e9_3 = executing(ex1AroundOutside).prints("around", "outside", "e1")
    def e10 = executing(ex1BeforeAfter).prints("before", "e1", "after")
    def e11 = executing(ex1BeforeAfterAround).prints("before", "around", "e1", "after")
    def e12 = executing(firstThenEx1).prints("first", "e1")
    def e13 = executeBodies(silentFirstThenEx1).map(_.message) must_== List("success")
    def e14 = executeBodies(failingFirstThenEx1).map(_.message) must_== List("error", "success")
 
  }
  case class compose() extends FragmentsExecution {
    def e1 = executing(ex1BeforeComposeBefore2).prints("before2", "before", "e1")
    def e2 = executing(ex1BeforeThenBefore2).prints("before", "before2", "e1")
    def e3 = executing(ex1AfterComposeAfter2).prints("e1", "after2", "after")
    def e4 = executing(ex1AfterThenAfter2).prints("e1", "after", "after2")
    def e5 = executing(ex1BeforeAfterComposeBefore2After2).prints("before2", "before", "e1", "after2", "after")
    def e6 = executing(ex1BeforeAfterThenBefore2After2).prints("before", "before2", "e1", "after", "after2")
    def e7 = executing(ex1AroundComposeAround2).prints("around2", "around", "e1")
    def e8 = executing(ex1AroundThenAround2).prints("around", "around2", "e1")
    def e9 = executing(ex1BeforeAfterAroundComposeBefore2After2Around2).
             prints("before2", "before", "around2", "around", "e1", "after2", "after")
    def e10 = executing(ex1BeforeAfterAroundThenBefore2After2Around2).
                   prints("before", "before2", "around", "around2", "e1", "after", "after2")
  }

  case class applyEach() extends FragmentsExecution {
    def e1 = {
      val spec = new Specification {
        implicit def before1 = before
        def is = "e1" ! { 1 must_== 1 }
      }
      executing(spec.content).prints("before")
    }
    def e2 = {
      val spec = new Spec {
        implicit def before1 = before
        "e1" in { 1 must_== 1 }
      }
      executing(spec.content).prints("before")
    }
    def e3 = {
      val spec = new Specification {
        implicit def outside1 = outsideInt
        def is = "e1" ! { (s: Int) => s must_== s }
      }
      executing(spec.content).prints("outside")
    }
    def e4 = {
      val spec = new Spec {
        implicit def outside1 = outsideInt
        "e1" in { (s: Int) => s must_== s }
      }
      executing(spec.content).prints("outside")
    }
  }

  case class mutableSpec() extends FragmentsExecution {
    def before1 = executing("e1" ! new beforeMutableContext { println("body"); 1 must_== 1 }).prints("before", "body")
    def after1  = executing("e1" ! new afterMutableContext { println("body"); 1 must_== 1 }).prints("body", "after")
    def around1 = executing("e1" ! new aroundMutableContext { println("body"); 1 must_== 1 }).prints("before", "body", "after")
  }

  class FragmentsExecution extends MockOutput with ContextData {
    def executing(exs: Fragments): Executed = Executed(executeBodies(exs))
    case class Executed(r: Seq[Result]) {
      def prints(ms: String*): Result = {
        messages must_== List(ms:_*)
      }  
    }
  }
}
trait ContextData extends StandardResults with FragmentsBuilder with ContextsForFragments with Contexts {

  def ok(name: String) = { println(name); success }
  def ok1 = ok("e1")
  def ok2 = ok("e2")
  
  def ex1 = "ex1" ! ok1  
  def ex1Before = "ex1" ! before(ok1) 
  def ex1BeforeComposeBefore2 = "ex1" ! (before compose before2)(ok1)  
  def ex1BeforeThenBefore2 = "ex1" ! (before then before2)(ok1)  
  def ex1AfterComposeAfter2 = "ex1" ! (after compose after2)(ok1)  
  def ex1AfterThenAfter2 = "ex1" ! (after then after2)(ok1)  
  def ex1BeforeAfterComposeBefore2After2 = "ex1" ! (beforeAfter compose before2After2)(ok1)  
  def ex1BeforeAfterThenBefore2After2 = "ex1" ! (beforeAfter then before2After2)(ok1)  
  def ex1AroundComposeAround2 = "ex1" ! (around compose around2)(ok1)  
  def ex1AroundThenAround2 = "ex1" ! (around then around2)(ok1)  
  def ex1BeforeAfterAroundComposeBefore2After2Around2 = "ex1" ! (beforeAfterAround compose before2After2Around2)(ok1)  
  def ex1BeforeAfterAroundThenBefore2After2Around2 = "ex1" ! (beforeAfterAround then before2After2Around2)(ok1)  

  def ex1_beforeFail = "ex1" ! beforeWithError(ok1) 
  def ex1_beforeSkipped = "ex1" ! beforeWithSkipped(ok1)
  def ex1_beforeMatchFailed = "ex1" ! beforeWithMatchFailed(ok1)
  def ex1_beforeSkippedThrown = "ex1" ! beforeWithSkippedThrown(ok1)
  def ex1_beforeMatchFailedThrown = "ex1" ! beforeWithMatchFailedThrown(ok1)
  def ex1_2Before = ex1Before ^ "ex2" ! before(ok2)

  def ex1After = "ex1" ! after(ok1) 
  def ex1FailAfter = "ex1" ! after(failure)
  def ex1_afterFail = "ex1" ! afterWithError(ok1)
  def ex1_2After = ex1After ^ "ex2" ! after(ok2)

  trait beforeContext extends Before {
    def before = println("before")
  }
  trait afterContext extends After {
    def after = println("after")
  }
  trait aroundContext extends Around {
    def around[R <% Result](r: =>R) = { println("before"); try { r } finally { println("after") }}
  }
  trait beforeMutableContext extends MBefore {
    def before = println("before")
  }
  trait afterMutableContext extends MAfter {
    def after = println("after")
  }
  trait aroundMutableContext extends MAround {
    def around[R <% Result](r: =>R) = { println("before"); try { r } finally { println("after") }}
  }


  def ex1ImplicitAfter = "ex1" ! ok1.after(println("after"))

  def ex1Around = "ex1" ! around(ok1) 
  def ex1Outside = "ex1" ! outside((s:String) => ok1)
  def ex1OutsideFail = "ex1" ! outsideWithError((s:String) => ok1)
  def ex1AroundOutside = "ex1" ! aroundOutside((s:String) => ok1)
  def ex1BeforeAfter = "ex1" ! beforeAfter(ok1)
  def ex1BeforeAfterAround = "ex1" ! beforeAfterAround(ok1)
  
  def firstThenEx1 = Step(println("first")) ^ ex1
  def silentFirstThenEx1 = Step("first") ^ ex1
  def failingFirstThenEx1 = Step { error("error"); 1 } ^ ex1
}
trait ContextsForFragments extends MockOutput {
  object before extends Before with Apply {
	  def before = println("before")
  }
  object beforeEach extends BeforeEach {
	  def before = println("before")
  }
  object before2 extends Before {
    def before = println("before2")
  }
  object beforeWithError extends Before with MockOutput {
	  def before = error("error")
  }
  object beforeWithSkipped extends Before with MockOutput {
	  def before = Skipped("skipped")
  }
  object beforeWithSkippedThrown extends Before with MockOutput with MustThrownMatchers {
	  def before = skipped("skipped")
  }
  object beforeWithMatchFailed extends Before with MockOutput with MustMatchers {
	  def before = 1 must_== 2
  }
  object beforeWithMatchFailedThrown extends Before with MockOutput with MustThrownMatchers {
	  def before = 1 must_== 2
  }
  object after extends After {
	  def after = println("after")
  }
  object after2 extends After {
    def after = println("after2")
  }
  object afterWithError extends After {
	  def after = error("error")
  }
  object around extends Around {
	  def around[T <% Result](a: =>T) = { println("around"); a } 
  }
  object around2 extends Around {
    def around[T <% Result](a: =>T) = { println("around2"); a } 
  }
  object outside extends Outside[String] {
	  def outside = { println("outside"); "string" }
  }
  object outsideInt extends Outside[Int] {
	  def outside = { println("outside"); 1 }
  }
  object outsideWithError extends Outside[String] with MockOutput {
	  def outside = { error("error"); "ok" }
  }
  object aroundOutside extends AroundOutside[String] {
	  def outside = { println("outside"); "string" }
    def around[T <% Result](a: =>T) = { println("around"); a }
  }
  object beforeAfter extends BeforeAfter {
	  def before = println("before")
	  def after = println("after")
  }
  object before2After2 extends BeforeAfter {
    def before = println("before2")
    def after = println("after2")
  }
  object beforeAfterAround extends BeforeAfterAround {
	  def before = println("before")
	  def after = println("after")
	  def around[T <% Result](a: =>T) = { println("around"); a } 
  }
  object before2After2Around2 extends BeforeAfterAround {
    def before = println("before2")
    def after = println("after2")
    def around[T <% Result](a: =>T) = { println("around2"); a } 
  }
}