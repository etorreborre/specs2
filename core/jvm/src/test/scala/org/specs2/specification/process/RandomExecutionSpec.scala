package org.specs2
package specification
package process

import scala.collection.mutable.ListBuffer
import fp.syntax._
import matcher._
import execute.{Result}
import specification.dsl.ExampleDsl
import specification.core.{Env, Fragments, OwnEnv}

class RandomExecutionSpec(val env: Env) extends script.Specification with Groups with ThrownExpectations with ExampleDsl with OwnEnv { def is = section("travis") ^ s2"""

 It is possible to force the order of execution of a specification so that
   + the execution is sequential but with a random order on the examples
   + the randomisation only happens in between steps

"""

  "random" - new group with results {
    eg := {
      val n = 10

      val spec = new Specification with RandomSequentialExecution { def is =
        s2"""${Fragments((1 to n).map(i => "e"+i ! ex(i)):_*)}"""

        def ex(i: =>Int) = { print("ex"+i); ok }
      }

      DefaultExecutor.runSpecificationAction(spec, ownEnv).runAction(ownEnv.executionEnv).as {
        val allExamples = allOf((1 to n).map("ex"+_):_*)

        messages must haveSize(10)
        messages must contain(allExamples)
        "the examples are executed randomly" ==> {
          messages must not (contain(allExamples).inOrder)
        }
      }.fold(execute.Error(_), identity)
    }

    eg := {
      val n = 10

      def ex(i: =>Int) = { print("ex"+i); ok }
      val fs_1_to_5 = Fragments((1 to 5).map(i => "e"+i ! ex(i)):_*)
      val fs_6_to_10 = Fragments((6 to 10).map(i => "e"+i ! ex(i)):_*)

      val spec = new Specification with RandomSequentialExecution { def is =
        s2"""${fs_1_to_5.append(step("stop")).append(fs_6_to_10)}"""
      }

      DefaultExecutor.runSpecificationAction(spec, ownEnv).runAction(ownEnv.executionEnv).as {

        val allExamples = allOf((1 to n).map("ex"+_):_*)

        messages must haveSize(10)
        messages must contain(allExamples)
        "the examples are executed randomly" ==> {
          messages must not (contain(allExamples).inOrder)
        }

        Result.foreach(1 to 5) { i =>
          messages.indexOf("ex"+i) must be_<(messages.indexOf("ex"+(i+5)))
        }
      }.fold(execute.Error(_), identity)
    }
  }

  trait results  {
    val messagesList = new ListBuffer[String]
    def messages = messagesList.toList
    def print(m: String) = synchronized { messagesList.append(m) }
  }
}
