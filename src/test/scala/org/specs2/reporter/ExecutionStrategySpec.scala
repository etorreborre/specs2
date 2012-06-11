package org.specs2
package reporter

import org.specs2.mutable
import specification.{SpecificationStructure, Step}
import io.MockOutput
import text.AnsiColors._

class ExecutionStrategySpec extends mutable.Specification {
  "If there is a step with stopOnFail and if one example in the previous block was not ok, then skip all other examples" >> {
    report(spec) === Seq(
      "spec",
      "+ ex1",
      "+ ex2",
      "x ex3",
      " ko",
      "o ex4",
      "o ex5",
      "Total for",
      "Finished in",
      "5 examples,")
  }

  def report(s: SpecificationStructure) = {
    val reporter = newReporter
    reporter.report(s)(args())
    reporter.textOutput.messages.map(m => removeColors(m).split(" ").take(2).mkString(" ")).filterNot(_.trim.isEmpty)
  }

  def newReporter = new ConsoleReporter {
    override lazy val textOutput = new TextResultOutput with MockOutput
    lazy val messages = textOutput.messages
  }

  val spec = new Specification { def is =
    "spec".title ^
    "ex1" ! ok ^
    "ex2" ! ok ^
    "ex3" ! ko ^
    Step(stopOnFail = true) ^
    "ex4" ! ok ^
    "ex5" ! ko ^
    end
  }
}