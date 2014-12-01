package org.specs2
package specification

import io._
import main.Arguments
import specification.core.{Env, Fragments}
import specification.create.FragmentsFactory
import specification.dsl._
import specification.process.DefaultExecutor
import sys._
import execute._
import matcher._
import _root_.org.specs2.mutable.{Around => MAround, Before => MBefore, After => MAfter, Specification => Spec}
import ExecutionVar._

class ContextSpec extends org.specs2.Spec with ResultMatchers with Groups { def is = s2"""

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

 The Before trait can be used to execute methods before Fragments
   the before method is executed before a first example                                                    ${g1().e1}
   the before method is executed before the second example                                                 ${g1().e2}

 If the before method throws an exception
   the first example will not execute                                                                      ${g1().e3}
   and it will be reported as an error                                                                     ${g1().e4}

 If the before method returns Skipped
   the first example will not execute                                                                      ${g1().e5}
   and it will be reported as skipped with the reason                                                      ${g1().e6}

 If the before method throws a SkippedException
   the first example will not execute                                                                      ${g1().e7}
   and it will be reported as skipped with the reason                                                      ${g1().e8}

 If the before method returns a MatchFailure
   the first example will not execute                                                                      ${g1().e9}
   and it will be reported as failed with the reason                                                       ${g1().e1}

 If the before method returns a MatchFailure
   the first example will not execute                                                                      ${g1().e1}
   and it will be reported as failed with the reason                                                       ${g1().e1}

 The After trait can be used to execute methods after Fragments
   the after method is executed after a first example                                                      ${g2().e1}
   the after method is executed after the second example                                                   ${g2().e2}
   if the expectation fails then the example must fail as well                                             ${g2().e3}

 If the after method throws an exception
   the first example will execute                                                                          ${g2().e4}
   but it will be reported as an error                                                                     ${g2().e5}

 The Around trait can be used to
   execute the example inside a user provided context                                                      ${g2().e6}


 Two traits can be mixed together and their effects combined
   BeforeAfter and Around                                                                                  ${g3().e1}

 An Action can be used to create Step fragments containing an action to execute:
   val beforeSpec = new Action
   def is = beforeSpec(c.println('beforeSpec')) ^ ex1

   that action will execute and return a result                                                            ${g4().e1}
   if it executes ok, nothing is printed, it is a silent Success                                           ${g4().e2}
   otherwise, it is reported as an Error                                                                   ${g4().e3}

 In a mutable spec
   the before code must be called before the body code                                                     ${g6().e1}
   the after code must be called after the body code                                                       ${g6().e2}
   the around code must be called around the body code                                                     ${g6().e3}
   the around method must rethrow failed results as exceptions                                             ${g6().e4}
                                                                                                           """

  implicit val arguments = main.Arguments()

  "before" - new g1 with FragmentsExecution {
    e1  := executing(ex1Before).prints("before", "e1")
    e2  := executing(ex1_2Before).prints("before", "e1", "before", "e2")
    e3  := executing(ex1_beforeFail).prints()
    e4  := executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
    e5  := executing(ex1_beforeSkipped).prints()
    e6  := executeBodies(ex1_beforeSkipped).map(_.message) must_== List("skipped")
    e7  := executing(ex1_beforeSkippedThrown).prints()
    e8  := executeBodies(ex1_beforeSkippedThrown).map(_.message) must_== List("skipped")
    e9  := executing(ex1_beforeMatchFailed).prints()
    e10 := executeBodies(ex1_beforeMatchFailed).map(_.message) must_== List("'1' is not equal to '2'")
    e11 := executing(ex1_beforeMatchFailedThrown).prints()
    e12 := executeBodies(ex1_beforeMatchFailedThrown).map(_.message) must_== List("'1' is not equal to '2'")
  }

  "context" - new g2 with FragmentsExecution {
    e1  := executing(ex1After).prints("e1", "after")
    e2  := executing(ex1_2After).prints("e1", "after", "e2", "after")
    e3  := executeBodies(ex1FailAfter).head must beFailing
    e4  := executing(ex1_afterFail).prints("e1")
    e5  := executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
    e6  := executing(ex1Around).prints("around", "e1")
  }

  "combination" - new g3 with FragmentsExecution {
    e1  := withEnv { env: Env =>
      abstract class ParentSpec extends Specification with BeforeAfterEach { def before = println("before"); def after = println("after") }
      abstract class ChildSpec extends ParentSpec with AroundEach { def around[R : AsResult](r: =>R) = { println("around"); AsResult(r) } }
      val child = new ChildSpec { def is =
        "e1" ! {println("e1"); ok}
      }

      executing(child.fragments(env)).prints("before", "around", "e1", "after")
    }
  }

  "other context" - new g4 with FragmentsExecution {
    e1 := executing(firstThenEx1).prints("first", "e1")
    e2 := executeBodies(silentFirstThenEx1).map(_.message) must_== List("", "success")
    e3 := executeBodies(failingFirstThenEx1).map(_.message) must_== List("error", "success")
  }

  "mutable contexts" - new g6 with FragmentsExecution with MustThrownExpectations {
    e1 := executing("e1" ! new beforeMutableContext { println("body"); 1 must_== 1 }).prints("before", "body")
    e2 := executing("e1" ! new afterMutableContext { println("body"); 1 must_== 1 }).prints("body", "after")
    e3 := executing("e1" ! new aroundMutableContext { println("body"); 1 must_== 1 }).prints("before", "body", "after")
    e4 := executing("e1" ! new aroundMutableContext { 1 must_== 2 }).results.head must beFailing
  }

  trait FragmentsExecution extends StringOutput with ContextData {
    def executeBodies(exs: Fragments) = {
      val env = Env(arguments = Arguments("sequential"))
      DefaultExecutor.executeSeq(exs.fragments)(env).map(_.executionResult)
    }

    def executing(exs: Fragments): Executed = Executed(executeBodies(exs))

    case class Executed(results: Seq[Result]) {
      def prints(ms: String*): Result = {
        val msgs = messages.toList
        (msgs must_== ms.toList).toResult
      }  
    }
  }
}

trait ContextData extends StandardResults with FragmentsFactory with ContextsForFragments with AcceptanceDsl {
  val factory = fragmentFactory

  def okValue(name: String) = { println(name); success }
  def ok1 = okValue("e1")
  def ok2 = okValue("e2")
  
  def ex1 = "ex1" ! ok1  
  def ex1Before = "ex1" ! before1(ok1)
  def ex1BeforeComposeBefore2 = "ex1" ! (before1 compose before2)(ok1)
  def ex1BeforeThenBefore2 = "ex1" ! (before1 andThen before2)(ok1)
  def ex1AfterComposeAfter2 = "ex1" ! (after1 compose after2)(ok1)

  def ex1_beforeFail = "ex1" ! beforeWithError(ok1) 
  def ex1_beforeSkipped = "ex1" ! beforeWithSkipped(ok1)
  def ex1_beforeMatchFailed = "ex1" ! beforeWithMatchFailed(ok1)
  def ex1_beforeSkippedThrown = "ex1" ! beforeWithSkippedThrown(ok1)
  def ex1_beforeMatchFailedThrown = "ex1" ! beforeWithMatchFailedThrown(ok1)
  def ex1_2Before = ex1Before ^ "ex2" ! before1(ok2)

  def ex1After = "ex1" ! after1(ok1)
  def ex1FailAfter = "ex1" ! after1(failure)
  def ex1_afterFail = "ex1" ! afterWithError(ok1)
  def ex1_2After = ex1After ^ "ex2" ! after1(ok2)

  trait beforeContext extends Before {
    def before = println("before")
  }
  trait afterContext extends After {
    def after = println("after")
  }
  trait aroundContext extends Around {
    def around[R : AsResult](r: =>R) = { println("before"); try { AsResult(r) } finally { println("after") }}
  }
  trait beforeMutableContext extends MBefore {
    def before = println("before")
  }
  trait afterMutableContext extends MAfter {
    def after = println("after")
  }
  trait aroundMutableContext extends MAround {
    def around[R : AsResult](r: =>R) = { println("before"); try { AsResult(r) } finally { println("after") }}
  }

  def ex1Around = "ex1" ! around1(ok1)

  def firstThenEx1 = step(println("first")) ^ ex1
  def silentFirstThenEx1 =  step("first") ^ ex1
  def failingFirstThenEx1 = step { error("error"); 1 } ^ ex1
}

trait ContextsForFragments extends StringOutput {
  object before1 extends Before {
    def before = println("before")
  }
  object before2 extends Before {
    def before = println("before2")
  }
  object beforeWithError extends Before with StringOutput {
    def before = error("error")
  }
  object beforeWithSkipped extends Before with StringOutput {
    def before = Skipped("skipped")
  }
  object beforeWithSkippedThrown extends Before with StringOutput with MustThrownMatchers {
    def before = skipped("skipped")
  }
  object beforeWithMatchFailed extends Before with StringOutput with MustMatchers {
    def before = 1 must_== 2
  }
  object beforeWithMatchFailedThrown extends Before with StringOutput with MustThrownMatchers {
    def before = 1 must_== 2
  }
  object after1 extends After {
    def after = println("after")
  }
  object after2 extends After {
    def after = println("after2")
  }
  object afterWithError extends After {
    def after = error("error")
  }
  object around1 extends Around {
    def around[T : AsResult](a: =>T) = { println("around"); AsResult(a) }
  }
  object around2 extends Around {
    def around[T : AsResult](a: =>T) = { println("around2"); AsResult(a) }
  }
  object fixtureInt extends Fixture[Int] {
    def apply[R : AsResult](f: Int => R) = {
      println("fixture")
      AsResult(f(1))
    }
  }
}