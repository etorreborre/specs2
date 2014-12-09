package org.specs2
package reporter

import org.specs2.mutable
import specification.{SpecificationStructure, Step}
import io.StringOutput
import text.AnsiColors._

class ExecutionStrategySpec extends mutable.Specification {

  "If there is a step with stopOnFail" >> {
    "if one example in the previous block was not ok, then skip all other examples" >> {
      report(spec1) must contain(allOf(
        "spec1",
        "+ ex1",
        "+ ex2",
        "x ex3",
        startWith("ko"),
        startWith("o ex4"),
        startWith("o ex5"))).inOrder
    }

    "if one example is ko, but not in the directly preceding block, then skip no examples" >> {
      report(spec2) must contain(allOf(
        "spec2",
        "x ex1",
        startWith("ko"),
        "+ ex2",
        "+ ex3",
        "+ ex4",
        "x ex5")).inOrder
    }

    "if the spec is sequential one example is ko, but not in the directly preceding block, then skip all examples" >> {
      report(spec3) must contain(allOf(
        "spec3",
        "x ex1",
        startWith("ko"),
        "+ ex2",
        "+ ex3",
        startWith("o ex4"),
        startWith("o ex5"))).inOrder
    }

    "a sequential specification must print examples as soon as they have been executed" >> {
      val reporter = newReporter

      val spec4 = new Specification { def is = sequential ^ "spec4".title ^ s2"""
        ex1  ${ print("e1") }
        ex2  ${ print("e2") }
        ex3  ${ print("e3") }
        ex4  ${ print("e4") }
        ex5  ${ print("e5") }
      """
        def print(s: String) = {
          Thread.sleep((math.random*100).toLong)
          reporter.textOutput.println(s); ok
        }
      }
      val reported = {
        reporter.report(spec4)(args())
        reporter.textOutput.messages.flatMap(m => removeColors(m).split("\n")).map(_.trim).filter(_.nonEmpty)
      }

      // an example must be printed before one of the next values is executed
      reported must contain(allOf(
        "+ ex2",
        "e5"
      )).inOrder
    }
  }

  def report(s: SpecificationStructure) = {
    val reporter = newReporter
    reporter.report(s)(args())
    reporter.textOutput.messages.flatMap(m => removeColors(m).split("\n")).map(_.trim).filter(_.nonEmpty)
  }

  def newReporter = new ConsoleReporter {
    override lazy val textOutput = new TextResultOutput with StringOutput
    lazy val messages = textOutput.messages
  }

  val spec1 = new Specification { def is = "spec1".title ^ s2"""
    ex1 $ok
    ex2 $ok
    ex3 $ko
    ${Step.stopOnFail}
    ex4 $ok
    ex5 $ko
    """
  }

  val spec2 = new Specification { def is = "spec2".title ^ s2"""
    ex1 $ko
    ${Step.stopOnFail(when = false)}
    ex2 $ok
    ex3 $ok
    ${Step.stopOnFail(when = true)}
    ex4 $ok
    ex5 $ko
    """
  }

  val spec3 = new Specification { def is = sequential ^ "spec3".title ^ s2"""
      ex1  $ko
      ${Step.stopOnFail(when = false)}
      ex2 $ok
      ex3 $ok
      ${Step.stopOnFail(when = true)}
      ex4 $ok
      ex5 $ko
    """
  }



}