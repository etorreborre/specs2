package org.specs2
package specification

import execute._

import scala.collection.mutable.ListBuffer
import matcher.{ResultMatchers, ThrownExpectations}

import scala.concurrent.duration._
import main.Arguments
import concurrent.ExecutionEnv
import specification.core._
import specification.process.DefaultExecutor
import control.producer._
import ResultMatchers._
import scala.concurrent.ExecutionContext

class ExecutorSpec(implicit ec: ExecutionContext) extends script.Specification with Groups with ThrownExpectations { def is = section("travis") ^ s2"""

 Steps
 =====
  + by step
  + stop on failed specified on a step
  + stop on skip specified in arguments
  + skipAll from arguments

 Execute
 =======
  + sequentially
  + with in-between steps
  + with a fatal execution error
  + stopOnFail and sequential

  with a timeout $timeOut
  with examples using an execution context $userEnv

"""

  import factory._

  "steps" - new group with results {

    eg := { env: Env =>
      val tf = env.arguments.execute.timeFactor

      val fragments = Seq(
        example("slow", slow(tf)),
        example("medium", medium(tf)),
        step(step1),
        example("fast", fast(tf)))

      execute(fragments, env) must not(contain(beSkipped[Result]))

      messages.toList must_== Seq("medium", "slow", "step", "fast")
    }

    eg := { env: Env =>
      val tf = env.arguments.execute.timeFactor

      val fragments = Seq(
        example("slow", slow(tf)),
        example("medium", mediumFail(tf)),
        step(step1).stopOnFail,
        example("fast", fast(tf)))

      execute(fragments, env) must contain(beSkipped[Result])

      messages.toList must_== Seq("medium", "slow", "step")
    }

    eg := { env: Env =>
      val tf = env.arguments.execute.timeFactor

      val fragments = Seq(
        example("slow", slow(tf)),
        example("medium", mediumSkipped(tf)),
        step(step1),
        example("fast", fast(tf)))

      execute(fragments, env.setArguments(Arguments("stopOnSkip"))) must contain(beSkipped[Result])

      messages.toList must_== Seq("medium", "slow", "step")
    }

    eg := { env: Env =>
      val tf = env.arguments.execute.timeFactor

      val fragments = Seq(
        example("ex1", fast(tf)),
        example("ex2", fast(tf)))

      execute(fragments, env.setArguments(Arguments("skipAll"))) must contain(beSkipped[Result])

      messages.toList must beEmpty
    }

  }

  "execute" - new group with results {

    eg := { env: Env =>
      val tf = env.arguments.execute.timeFactor

      val fragments = Seq(
        example("slow", slow(tf)),
        example("medium", medium(tf)),
        step(step1),
        example("fast", fast(tf)))

      execute(fragments, env.setArguments(Arguments("sequential"))) must not(contain(beSkipped[Result]))

      messages.toList must_== Seq("slow", "medium", "step", "fast")
    }

    eg := { env: Env =>
      val tf = env.arguments.execute.timeFactor

      val fragments = Seq(
        example("slow", slow(tf)),
        example("medium", medium(tf)),
        step(step1),
        example("fast", fast(tf)))

      execute(fragments, env.setArguments(Arguments("sequential")))

      messages.toList must_== Seq("slow", "medium", "step", "fast")
    }

    eg := { env: Env =>

      val fragments = Seq(
        example("fast1", ok("ok1")),
        step(fatalStep),
        example("fast2", ok("ok2")))

      execute(fragments, env)

      messages.toList must_== Seq("ok1", "fatal")
    }

    eg := { env: Env =>

      val fragments = Seq(
        example("e1", ko("ko1")),
        example("e2", ok("ok2")))

      val env1 = env.setArguments(Arguments.split("sequential stopOnFail"))
      val results = execute(fragments, env1).map(_.status)

      results must contain("x", "o")
    }
  }

  def timeOut = { env: Env =>
    val timeFactor = env.arguments.execute.timeFactor

    val messages = new ListBuffer[String]
    def verySlow = { Thread.sleep(600 * timeFactor.toLong); messages.append("very slow"); success }

    val fragments = Seq(example("very slow", verySlow))
    val env1 = env.setTimeout(100.millis * timeFactor.toLong)

    execute(fragments, env1) must contain(beSkipped[Result]("timeout after "+100*timeFactor+" milliseconds"))
  }

  def userEnv = { env: Env =>
    val fragments =
      Fragments.foreach(1 to 10) { i: Int =>
        "test " + i ! Execution.withExecutionEnv { implicit ec: ExecutionEnv =>
          scala.concurrent.Future(1) must be_==(1).await
        }
      }
    execute(fragments.fragments, env) must contain(beSuccessful[Result]).forall
  }

  val factory = fragmentFactory

  def execute(fragments: Seq[Fragment], env: Env): List[Result] =
    DefaultExecutor.execute1(env)(Fragments(fragments:_*).contents).runList.runOption.toList.flatten.map(_.executionResult)

  trait results {
    val messages = new ListBuffer[String]

    // this cannot be made lazy vals otherwise this will block on 'slow'
    def ok(name: String)              = { messages.append(name); success }
    def ko(name: String)              = { messages.append(name); failure }
    def fast(timeFactor: Int)         = { messages.append("fast"); success }
    def medium(timeFactor: Int)       = { Thread.sleep(10 * timeFactor.toLong);  messages.append("medium"); success }
    def ex(s: String)                 = { messages.append(s); success }
    def mediumFail(timeFactor: Int)   = { Thread.sleep(10 * timeFactor.toLong);  messages.append("medium"); failure }
    def mediumSkipped(timeFactor: Int)= { Thread.sleep(10 * timeFactor.toLong);  messages.append("medium"); skipped }
    def slow(timeFactor: Int)         = { Thread.sleep(200 * timeFactor.toLong); messages.append("slow");   success }
    def verySlow(timeFactor: Int)     = { Thread.sleep(600 * timeFactor.toLong); messages.append("very slow"); success }
    def step1                         = { messages.append("step");   success }
    def fatalStep                     = { messages.append("fatal");  if (true) throw new java.lang.Error("fatal error!"); success }
  }

}
