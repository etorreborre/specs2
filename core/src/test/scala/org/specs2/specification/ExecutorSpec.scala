package org.specs2
package specification

import execute._
import scala.collection.mutable.ListBuffer
import matcher.{ThrownExpectations, ResultMatchers}
import scala.concurrent.duration._
import main.Arguments
import specification.core._
import specification.process.DefaultExecutor

class ExecutorSpec extends script.Specification with Groups with ResultMatchers with ThrownExpectations { def is = s2"""

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

  with a timeout $timeOut ${tag("travis")}

"""

  import factory._

  "steps" - new group with results {

    eg := { env: Env =>
      val fragments = Seq(
        example("slow", slow),
        example("medium", medium),
        step(step1),
        example("fast", fast))

      execute(fragments, env) must not(contain(beSkipped[Result]))

      messages.toList must_== Seq("medium", "slow", "step", "fast")
    }

    eg := { env: Env =>
      val fragments = Seq(
        example("slow", slow),
        example("medium", mediumFail),
        step(step1).stopOnFail,
        example("fast", fast))

      execute(fragments, env) must contain(beSkipped[Result])

      messages.toList must_== Seq("medium", "slow", "step")
    }

    eg := { env: Env =>
      val fragments = Seq(
        example("slow", slow),
        example("medium", mediumSkipped),
        step(step1),
        example("fast", fast))

      execute(fragments, env.setArguments(Arguments("stopOnSkip"))) must contain(beSkipped[Result])

      messages.toList must_== Seq("medium", "slow", "step")
    }

    eg := { env: Env =>
      val fragments = Seq(
        example("ex1", fast),
        example("ex2", fast))

      execute(fragments, env.setArguments(Arguments("skipAll"))) must contain(beSkipped[Result])

      messages.toList must beEmpty
    }

  }

  "execute" - new group with results {

    eg := { env: Env =>
      val fragments = Seq(
        example("slow", slow),
        example("medium", medium),
        step(step1),
        example("fast", fast))

      execute(fragments, env.setArguments(Arguments("sequential"))) must not(contain(beSkipped[Result]))

      messages.toList must_== Seq("slow", "medium", "step", "fast")
    }

    eg := { env: Env =>
      val fragments = Seq(
        example("slow", slow),
        example("medium", medium),
        step(step1),
        example("fast", fast))

      execute(fragments, env.setArguments(Arguments("sequential")))

      messages.toList must_== Seq("slow", "medium", "step", "fast")
    }
  }

  def timeOut = { env: Env =>
    val messages = new ListBuffer[String]
    def verySlow      = { Thread.sleep(600); messages.append("very slow"); success }

    val fragments = Seq(example("very slow", verySlow))
    val env1 = env.copy(executionEnv = env.executionEnv.setTimeout(100.millis))

    execute(fragments, env1) must contain(beSkipped[Result]("timeout after 100 milliseconds"))
  }


  val factory = fragmentFactory

  def execute(fragments: Seq[Fragment], env: Env): IndexedSeq[Result] =
    DefaultExecutor.execute1(env)(Fragments(fragments:_*).contents).runLog.run.map(_.executionResult)

  trait results {
    val messages = new ListBuffer[String]

    // this cannot be made lazy vals otherwise this will block on 'slow'
    def fast          = {                    messages.append("fast"); success }
    def medium        = { Thread.sleep(10);  messages.append("medium"); success }
    def ex(s: String) = { messages.append(s); success }
    def mediumFail    = { Thread.sleep(10);  messages.append("medium"); failure }
    def mediumSkipped = { Thread.sleep(10);  messages.append("medium"); skipped }
    def slow          = { Thread.sleep(200); messages.append("slow");   success }
    def verySlow      = { Thread.sleep(600); messages.append("very slow"); success }
    def step1         = { Thread.sleep(0);  messages.append("step");   success }
  }



}
