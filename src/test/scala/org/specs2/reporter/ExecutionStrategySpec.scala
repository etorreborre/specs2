package org.specs2
package reporter

import org.specs2.mutable
import specification.{SpecificationStructure, Step}
import io.StringOutput
import text.AnsiColors._

class ExecutionStrategySpec extends mutable.Specification {

  "If there is a step with stopOnFail" >> {
    "if one example in the previous block was not ok, then skip all other examples" >> {
      report(spec1) must contain(
        "spec1",
        "+ ex1",
        "+ ex2",
        "x ex3",
        " ko",
        "o ex4",
        "o ex5").inOrder
    }

    "if one example is ko, but not in the directly preceding block, then skip no examples" >> {
      report(spec2) must contain(
        "spec2",
        "x ex1",
        " ko",
        "+ ex2",
        "+ ex3",
        "+ ex4",
        "x ex5").inOrder
    }

    "if the spec is sequential one example is ko, but not in the directly preceding block, then skip all examples" >> {
      report(spec3) must contain(
        "spec3",
        "x ex1",
        " ko",
        "+ ex2",
        "+ ex3",
        "o ex4",
        "o ex5").inOrder
    }
  }

  def report(s: SpecificationStructure) = {
    val reporter = newReporter
    reporter.report(s)(args())
    reporter.textOutput.messages.map(m => removeColors(m).split(" ").take(2).mkString(" ")).filterNot(_.trim.isEmpty)
  }

  def newReporter = new ConsoleReporter {
    override lazy val textOutput = new TextResultOutput with StringOutput
    lazy val messages = textOutput.messages
  }

  val spec1 = new Specification { def is =
    "spec1".title ^
    "ex1" ! ok ^
    "ex2" ! ok ^
    "ex3" ! ko ^
    Step(stopOnFail = true) ^
    "ex4" ! ok ^
    "ex5" ! ko ^ end
  }

  val spec2 = new Specification { def is =
    "spec2".title ^
    "ex1" ! ko ^
    step() ^
    "ex2" ! ok ^
    "ex3" ! ok ^
    Step(stopOnFail = true) ^
    "ex4" ! ok ^
    "ex5" ! ko ^ end
  }

  val spec3 = new Specification { def is = sequential ^
    "spec3".title ^
      "ex1" ! ko ^
      step() ^
      "ex2" ! ok ^
      "ex3" ! ok ^
      Step(stopOnFail = true) ^
      "ex4" ! ok ^
      "ex5" ! ko ^ end
  }

}