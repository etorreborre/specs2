package org.specs2
package specification
package process

import scala.collection.mutable.ListBuffer
import matcher.*
import ExpectationsDescription.*
import execute.{Result}
import specification.dsl.ExampleDsl
import specification.core.*
import org.specs2.text.Trim.*

class RandomExecutionSpec(val env: Env) extends Specification with ThrownExpectations with ExampleDsl with OwnEnv {
  def is = section("ci") ^ s2"""

 It is possible to force the order of execution of a specification so that
   the execution is sequential but with a random order on the examples $random1
   the randomisation only happens in between steps $random2
"""

  import fp.syntax.{given}

  def random1 =
    val results = Results()
    import results.*

    val n = 10

    val spec = new Specification {
      def is = sequentialRandom ^
        s2"""${Fragments((0 until n).map(i => "e" + i ! ex(i))*)}"""

      def ex(i: =>Int) =
        print(">>> ex" + i)
        Thread.sleep(i * 10)
        print("<<< ex" + i)
        ok("ex" + i)
    }

    DefaultExecutor
      .runSpecificationAction(spec, ownEnv)
      .runAction(ownEnv.executionEnv)
      .map { _ =>
        val allExamples = allOf((0 until n).flatMap(i => List(">>> ex" + i, "<<< ex" + i))*)

        messages must haveSize(20)
        messages must contain(allExamples)
        "the examples are executed randomly" ==> {
          messages must not(contain(allExamples).inOrder)
        }
        "the examples are executed sequentially" ==> {
          messages.map(_.take(6)) === (0 until n).flatMap(_ => List(">>> ex", "<<< ex"))
        }
      }
      .fold(execute.Error(_), identity)

  def random2 =
    val results = Results()
    import results.*

    val n = 10
    def ex(i: =>Int) = { print("ex" + i); ok }
    val fs_1_to_5 = Fragments((1 to 5).map(i => "e" + i ! ex(i))*)
    val fs_6_to_10 = Fragments((6 to 10).map(i => "e" + i ! ex(i))*)

    val spec = new Specification {
      def is = sequentialRandom ^
        s2"""${fs_1_to_5.append(step(ok)).append(fs_6_to_10)}"""
    }
    DefaultExecutor
      .runSpecificationAction(spec, ownEnv)
      .runAction(ownEnv.executionEnv)
      .map { _ =>
        val allExamples = allOf[String]((1 to n).map("ex" + _)*)

        messages must haveSize(10)
        messages must contain(allExamples)

        "the examples are executed randomly" ==> {
          messages must not(contain(allExamples).inOrder)
        }

        Result.foreach(1 to 5) { i =>
          messages.indexOf("ex" + i) must be_<(messages.indexOf("ex" + (i + 5)))
        }
      }
      .fold(execute.Error(_), identity)

  case class Results():
    val messagesList = new ListBuffer[String]
    def messages = messagesList.toList
    def print(m: String) = synchronized { messagesList.append(m) }
}
