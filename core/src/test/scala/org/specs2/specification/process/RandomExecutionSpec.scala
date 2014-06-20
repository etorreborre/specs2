package org.specs2
package specification
package process

import matcher.ThrownExpectations
import specification.core.Fragments
import scala.collection.mutable.ListBuffer

class RandomExecutionSpec extends script.Specification with Groups with ThrownExpectations { def is = s2"""
 It is possible to force the order of execution of a specification so that
   + the execution is sequential but with a random order on the examples
   + the randomisation only happens in between steps

"""

  "random" - new group with results {
    eg := {
      val n = 10
      val spec = new Specification with RandomExecution { def is =
        s2"""${Fragments((1 to n).map(i => "e"+i ! ex(i)):_*)}"""

        def ex(i: =>Int) = { print("ex"+i); ok }
      }

      Executor.runSpecification(spec)

      val allExamples = allOf((1 to n).map("ex"+_):_*)

      messages must haveSize(10)
      messages must contain(allExamples)
      messages must not (contain(allExamples).inOrder)
    }

    eg := {
      val n = 10

      def ex(i: =>Int) = { print("ex"+i); ok }
      val fs_1_to_5 = Fragments((1 to 5).map(i => "e"+i ! ex(i)):_*)
      val fs_6_to_10 = Fragments((6 to 10).map(i => "e"+i ! ex(i)):_*)

      val spec = new Specification with RandomExecution { def is =
        s2"""${fs_1_to_5.append(step("stop")).append(fs_6_to_10)}"""
      }

      Executor.runSpecification(spec)

      val allExamples = allOf((1 to n).map("ex"+_):_*)

      messages must haveSize(10)
      messages must contain(allExamples)
      messages must not (contain(allExamples).inOrder)

      (1 to 5) must contain ((i: Int) => messages.indexOf("ex"+i) must be_<(messages.indexOf("ex"+(i+5)))).forall
    }
  }

  trait results  {
    val messages = new ListBuffer[String]
    def print(m: String) = { messages.append(m) }
  }
}
