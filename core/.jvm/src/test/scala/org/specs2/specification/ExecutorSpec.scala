package org.specs2
package specification

import execute.*

import scala.collection.mutable.ListBuffer
import matcher.{ResultMatchers, ThrownExpectations}

import scala.concurrent.duration.*
import main.Arguments
import concurrent.ExecutionEnv
import specification.core.*
import specification.process.DefaultExecutor
import fp.*, syntax.*
import ResultMatchers.*
import scala.concurrent.*

class ExecutorSpec(val env: Env) extends Specification with ThrownExpectations with OwnEnv { def is = sequential ^ section("ci") ^ s2"""

 Steps
 =====
  by step $e1
  stop on failed specified on a step $e2
  stop on error specified on a step $e3
  stop on skip specified in arguments $e4
  stop on failed with a sequential specification $e5
  skipAll from arguments $e6

 Execute
 =======
  sequentially $e7
  with in-between steps $e8
  with a fatal execution error $e9
  with a fatal execution error in a step $e10
  stopOnFail and sequential $e11

  with a timeout $timeout
  with examples using an execution context $userEnv

 Time
 ===

  the timer must be started for each execution $e12
  the timer must be started for each sequential execution $e13
  the timer must be started for each skipped execution $e14

"""

  import factory.*

  def e1 =
    val results = Results(); import results.*
    val tf = ownEnv.arguments.execute.timeFactor
    val fragments = Fragments(
      example("slow", slow(tf)),
      example("medium", medium(tf)),
      step(step1),
      example("fast", fast(tf)))
    execute(fragments, ownEnv) must not(contain(beSkipped[Result]))
    messages.toList must ===(Seq("medium", "slow", "step", "fast"))

  def e2 =
    val results = Results(); import results.*
    val tf = ownEnv.arguments.execute.timeFactor
    val fragments = Fragments(
      example("slow", slow(tf)),
      example("medium", mediumFail(tf)),
      step(step1).stopOnFail,
      example("fast", fast(tf)))
    execute(fragments, ownEnv) must contain(beSkipped[Result])
    messages.toList must ===(Seq("medium", "slow", "step"))

  def e3 =
    val results = Results(); import results.*
    val tf = ownEnv.arguments.execute.timeFactor
    val fragments = Fragments(
      example("slow", slow(tf)),
      example("medium", mediumError(tf)),
      step(step1).stopOnError,
      example("fast", fast(tf)))
    execute(fragments, ownEnv) must contain(beSkipped[Result])
    messages.toList must ===(Seq("medium", "slow", "step"))

  def e4 =
    val results = Results(); import results.*
    val tf = ownEnv.arguments.execute.timeFactor
    val fragments = Fragments(
      example("slow", slow(tf)),
      example("medium", mediumSkipped(tf)),
      step(step1),
      example("fast", fast(tf)))
    execute(fragments, ownEnv.setArguments(Arguments("stopOnSkip"))) must contain(beSkipped[Result])
    messages.toList must ===(Seq("medium", "slow", "step"))

  def e5 =
    val results = Results(); import results.*
    val tf = ownEnv.arguments.execute.timeFactor
    val fragments = Fragments(
      example("slow", slow(tf)),
      example("medium", mediumFail(tf)),
      example("fast", fast(tf)))
    execute(fragments, ownEnv.setArguments(Arguments("stopOnFail", "sequential"))) must contain(beFailing[Result])
    messages.toList must ===(Seq("slow", "medium"))

  def e6 =
    val results = Results(); import results.*
    val tf = ownEnv.arguments.execute.timeFactor
    val fragments = Fragments(
      example("ex1", fast(tf)),
      example("ex2", fast(tf)))
    execute(fragments, ownEnv.setArguments(Arguments("skipAll"))) must contain(beSkipped[Result])
    messages.toList must beEmpty

  def e7 =
    val results = Results(); import results.*
    val tf = ownEnv.arguments.execute.timeFactor
    val fragments = Fragments(
      example("slow", slow(tf)),
      example("medium", medium(tf)),
      step(step1),
      example("fast", fast(tf)))
    execute(fragments, ownEnv.setArguments(Arguments("sequential"))) must not(contain(beSkipped[Result]))
    messages.toList must ===(Seq("slow", "medium", "step", "fast"))

  def e8 =
    val results = Results(); import results.*
    val tf = ownEnv.arguments.execute.timeFactor
    val fragments = Fragments(
      example("slow", slow(tf)),
      example("medium", medium(tf)),
      step(step1),
      example("fast", fast(tf)))
    execute(fragments, ownEnv.setArguments(Arguments("sequential")))
    messages.toList must ===(Seq("slow", "medium", "step", "fast"))

  def e9 =
    val results = Results()
    val fragments = Fragments(
      example("fast1", results.ok("ok1")),
      step(results.fatalStep),
      example("fast2", results.ok("ok2")))
    execute(fragments, ownEnv)
    results.messages.toList must ===(Seq("ok1", "fatal"))

  def e10 =
    val fragments = Fragments(
      step(throw new Exception("fatal")),
      example("e1", ok("ok")),
      step(throw new Exception("fatal")))
    val rs = execute(fragments, ownEnv).map(_.status)
    rs must contain("!", "o", "!")

  def e11 =
    val fragments = Fragments(
      example("e1", ko("ko1")),
      example("e2", ok("ok2")))
    val env1 = ownEnv.setArguments(Arguments.split("sequential stopOnFail"))
    val rs = execute(fragments, env1).map(_.status)
    rs must contain("x", "o")

  def tf = ownEnv.arguments.execute.timeFactor

  def fragments(results: Results) =
    import results.*
    Fragments(
    example("slow", slow(tf)),
    example("medium", medium(tf)),
    example("fast", fast(tf)))

  def e12 =
    val results = Results()
    val times = executionTimes(fragments(results), ownEnv)
    times must containMatch("(\\d)+ ms")

  def e13 =
    val results = Results()
    val times = executionTimes(fragments(results), ownEnv.setArguments(Arguments("sequential")))
    times must containMatch("(\\d)+ ms")

  def e14 =
    val results = Results()
    val times = executionTimes(fragments(results), ownEnv.setArguments(Arguments("skipAll")))
    times must containMatch("(\\d)+ ms")

  def timeout =
    val timeFactor = ownEnv.arguments.execute.timeFactor

    val messages = new ListBuffer[String]
    def verySlow: Result = { Thread.sleep(600 * timeFactor.toLong); messages.append("very slow"); success }

    val fragments = Fragments(example("very slow", verySlow))
    val env1 = ownEnv.setTimeout(100.millis * timeFactor.toLong)

    execute(fragments, env1) must contain(beFailing[Result]("timeout after "+100*timeFactor+" milliseconds"))

  def userEnv =
    val fragments =
      Fragments.foreach(1 to 2) { (i: Int) =>
        "test " + i ! Execution.withExecutionEnv { (ee: ExecutionEnv) =>
          Await.result(scala.concurrent.Future(1)(ee.executionContext), 5.second) === 1
        }
      }
    val e = Env()
    try execute(fragments, e) must contain(beSuccessful[Result]).forall
    finally e.shutdown()

  final lazy val factory = fragmentFactory

  def runFragments(fragments: Fragments, env: Env): List[Fragment] =
    DefaultExecutor(env).execute(env.arguments)(fragments.contents).runList.
      runOption(env.executionEnv).toList.flatten

  def execute(fragments: Fragments, env: Env): List[Result] =
    runFragments(fragments, env).traverse(_.executionResult).run(env.executionEnv)

  def executionTimes(fragments: Fragments, env: Env): List[String] =
    Traverse.listInstance.traverse(runFragments(fragments, env))((f: Fragment) => f.executedResult.map(_.timer.time)).run(env.executionEnv)

  def executions(fragments: Fragments, env: Env): List[Execution] =
    runFragments(fragments, env).map(_.execution)

  case class Results():
    val messages = new ListBuffer[String]

    // this cannot be made lazy vals otherwise this will block on 'slow'
    def ok(name: String)              : Result = { messages.append(name); success }
    def ko(name: String)              : Result = { messages.append(name); failure }
    def fast(timeFactor: Int)         : Result = { messages.append("fast"); success }
    def medium(timeFactor: Int)       : Result = { Thread.sleep(100 * timeFactor.toLong);  messages.append("medium"); success }
    def ex(s: String)                 : Result = { messages.append(s); success }
    def mediumFail(timeFactor: Int)   : Result = { Thread.sleep(10 * timeFactor.toLong);  messages.append("medium"); failure }
    def mediumError(timeFactor: Int)  : Result = { Thread.sleep(10 * timeFactor.toLong);  messages.append("medium"); anError }
    def mediumSkipped(timeFactor: Int): Result = { Thread.sleep(10 * timeFactor.toLong);  messages.append("medium"); skipped }
    def slow(timeFactor: Int)         : Result = { Thread.sleep(1000 * timeFactor.toLong); messages.append("slow");   success }
    def verySlow(timeFactor: Int)     : Result = { Thread.sleep(2000 * timeFactor.toLong); messages.append("very slow"); success }
    def step1                         : Result = { messages.append("step");   success }
    def fatalStep                     : Result = { messages.append("fatal");  if true then throw new java.lang.Error("fatal error!"); success }

}
