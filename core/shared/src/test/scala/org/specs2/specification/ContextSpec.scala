package org.specs2
package specification

import io._
import main.Arguments
import specification.core.{Env, Fragments, Fragment, OwnEnv}
import specification.create.FragmentsFactory
import specification.dsl._
import specification.process.DefaultExecutor
import sys._
import execute._
import matcher._
import fp.syntax._

case class ContextSpec(env: Env) extends Spec with ResultMatchers with OwnEnv { def is = s2"""

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
   the before method is executed before a first example                                                    $before1
   the before method is executed before the second example                                                 $before2

 If the before method throws an exception
   the first example will not execute                                                                      $before3
   and it will be reported as an error                                                                     $before4

 If the before method returns Skipped
   the first example will not execute                                                                      $before5
   and it will be reported as skipped with the reason                                                      $before6

 If the before method throws a SkippedException
   the first example will not execute                                                                      $before7
   and it will be reported as skipped with the reason                                                      $before8

 If the before method returns a MatchFailure
   the first example will not execute                                                                      $before9
   and it will be reported as failed with the reason                                                       $before10

 If the before method returns a MatchFailure
   the first example will not execute                                                                      $before11
   and it will be reported as failed with the reason                                                       $before12

 The After trait can be used to execute methods after Fragments
   the after method is executed after a first example                                                      $after1
   the after method is executed after the second example                                                   $after2
   if the expectation fails then the example must fail as well                                             $after3

 If the after method throws an exception
   the first example will execute                                                                          $after4
   but it will be reported as an error                                                                     $after5

 The Around trait can be used to
   execute the example inside a user provided context                                                      $around1

 Two traits can be mixed together and their effects combined
   BeforeAfter and Around                                                                                  $combined1

 An Action can be used to create Step fragments containing an action to execute:
   val beforeSpec = new Action
   def is = beforeSpec(c.println('beforeSpec')) ^ ex1

   that action will execute and return a result                                                            $step1
   if it executes ok, nothing is printed, it is a silent Success                                           $step2
   otherwise, it is reported as an Error                                                                   $step3

"""

  given arguments as Arguments = main.Arguments()

  def before1  = { val d = data(); d.executing(d.ex1Before).prints("before", "e1") }
  def before2  = { val d = data(); d.executing(d.ex1_2Before).prints("before", "e1", "before", "e2") }
  def before3  = { val d = data(); d.executing(d.ex1_beforeFail).prints() }
  def before4  = { val d = data(); d.executeBodies(d.ex1_beforeFail).map(_.message) must ===(List("java.lang.RuntimeException: error")) }
  def before5  = { val d = data(); d.executing(d.ex1_beforeSkipped).prints() }
  def before6  = { val d = data(); d.executeBodies(d.ex1_beforeSkipped).map(_.message) must ===(List("skipped")) }
  def before7  = { val d = data(); d.executing(d.ex1_beforeSkippedThrown).prints() }
  def before8  = { val d = data(); d.executeBodies(d.ex1_beforeSkippedThrown).map(_.message) must ===(List("skipped")) }
  def before9  = { val d = data(); d.executing(d.ex1_beforeMatchFailed).prints() }
  def before10 = { val d = data(); d.executeBodies(d.ex1_beforeMatchFailed).map(_.message) must ===(List("1 != 2")) }
  def before11 = { val d = data(); d.executing(d.ex1_beforeMatchFailedThrown).prints() }
  def before12 = { val d = data(); d.executeBodies(d.ex1_beforeMatchFailedThrown).map(_.message) must ===(List("1 != 2")) }
  def after1 =  { val d = data(); d.executing(d.ex1After).prints("e1", "after") }
  def after2 =  { val d = data(); d.executing(d.ex1_2After).prints("e1", "after", "e2", "after") }
  def after3 =  { val d = data(); d.executeBodies(d.ex1FailAfter).head must beFailing }
  def after4 =  { val d = data(); d.executing(d.ex1_afterFail).prints("e1") }
  def after5 =  { val d = data(); d.executeBodies(d.ex1_beforeFail).map(_.message) must ===(List("java.lang.RuntimeException: error")) }
  def around1 = { val d = data(); d.executing(d.ex1Around).prints("around", "e1") }

  def combined1 =
    val d = data(); import d._

    abstract class ParentSpecification extends Specification with BeforeAfterEach { def before = println("before"); def after = println("after") }
    abstract class ChildSpecification extends ParentSpecification with AroundEach { def around[R : AsResult](r: =>R) = { println("around"); AsResult(r) } }
    val child = new ChildSpecification { def is =
      "e1" ! {println("e1"); ok}
    }
    executing(child.fragments(env)).prints("before", "around", "e1", "after")

  def step1 = { val d = data(); d.executing(d.firstThenEx1).prints("first", "e1") }
  def step2 = { val d = data(); d.executeBodies(d.silentFirstThenEx1).map(_.message) must ===(List("", "success")) }
  def step3 = { val d = data(); d.executeBodies(d.failingFirstThenEx1).map(_.message) must ===(List("org.specs2.specification.core.FatalExecution: error", "")) }

  case class data() extends StringOutput with ContextData:
    def executeBodies(ex: Fragment): List[Result] =
      executeBodies(Fragments(ex))

    def executeBodies(exs: Fragments): List[Result] =
      val env = Env(arguments = Arguments("sequential"))
      try DefaultExecutor.executeFragments(exs)(env).traverse(_.executionResult).run(env.executionEnv)
      finally env.shutdown()

    def executing(exs: Fragments): Executed = Executed(executeBodies(exs))
    def executing(ex: Fragment): Executed = Executed(executeBodies(ex))

    case class Executed(results: Seq[Result]):
      def prints(ms: String*): Result =
        (messages must ===(ms.toList)).toResult
}

trait ContextData extends StandardResults with FragmentsFactory with ContextsForFragments with AcceptanceDsl:
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

  trait beforeContext extends Before:
    def before = println("before")
  trait afterContext extends After:
    def after = println("after")
  trait aroundContext extends Around:
    def around[R : AsResult](r: =>R) = { println("before"); try { AsResult(r) } finally { println("after") }}

  def ex1Around: Fragments = "ex1" ! around1(ok1)

  def firstThenEx1 = step(println("first")) ^ ex1
  def silentFirstThenEx1 =  step("first") ^ ex1
  def failingFirstThenEx1 = step { error("error"); 1 } ^ ex1

trait ContextsForFragments extends StringOutput:
  object before1 extends Before:
    def before = println("before")
  object before2 extends Before:
    def before = println("before2")
  object beforeWithError extends Before with StringOutput:
    def before = error("error")
  object beforeWithSkipped extends Before with StringOutput:
    def before = Skipped("skipped")
  object beforeWithSkippedThrown extends Before with StringOutput with MustThrownMatchers:
    def before = skipped("skipped")
  object beforeWithMatchFailed extends Before with StringOutput with MustThrownMatchers:
    def before = 1 must ===(2)
  object beforeWithMatchFailedThrown extends Before with StringOutput with MustThrownMatchers:
    def before = 1 must ===(2)
  object after1 extends After:
    def after = println("after")
  object after2 extends After:
    def after = println("after2")
  object afterWithError extends After:
    def after = error("error")
  object around1 extends Around:
    def around[T : AsResult](a: =>T) = { println("around"); AsResult(a) }
  object around2 extends Around:
    def around[T : AsResult](a: =>T) = { println("around2"); AsResult(a) }
  object fixtureInt extends Fixture[Int]:
    def apply[R : AsResult](f: Int => R) =
      println("fixture")
      AsResult(f(1))
