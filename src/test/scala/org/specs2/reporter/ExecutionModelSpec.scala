package org.specs2
package reporter

import io._
import specification._
import execute.Executable

class ExecutionModelSpec extends Specification with ScalaCheck with Groups { def is =

  "A sequential specification must always be reported with a strict order of its fragments"                             ! g1().e1^
  "A concurrent specification must have at least one execution where the order is changed"                              ! g1().e2^
  "A Step must always be executed after the preceding examples "                                                        ! g1().e3^
                                                                                                                        end

  implicit val timedSpec = SpecificationData.arbTimedSpecification(50)
  implicit val arguments = args.store(never=true)

  "examples" - new g1 with Report {

    e1 := prop { spec: Specification =>
      val reporter = newReporter
      reporter.report(simpleLabel(spec, reporter.textOutput))(arguments <| args(sequential=true))
      reporter.outputLabels must beSorted
    }.set(minSize = 3, maxSize = 7, workers = 4)

    e2 := prop { spec: Specification =>
      val reporter = newReporter
      reporter.report(simpleLabel(spec, reporter.textOutput))
      reporter.outputLabels must beSorted
    }.set(minSize = 3, maxSize = 7).not

    e3 := prop { spec: Specification =>
      val reporter = newReporter
      val s = exampleAndStepLabel(spec, reporter.textOutput)
      reporter.report(s)

      forall(reporter.outputLabels.inits.toSeq.filterNot(_.isEmpty)) { labels =>
        if (labels.head.startsWith("step")) {
          val numbers = labels.map(_.replace("step ", "").replace("example", "").trim.toInt)
          forall(numbers.drop(1))(l => l aka "with labels: "+labels must be_>(numbers.head))
        }
        else ok
      }
    }.set(minSize = 3, maxSize = 8, minTestsOk = 50)
  }

  trait Report {
    def newReporter = new ConsoleReporter {
      override lazy val textOutput = new TextResultOutput with MockOutput
      lazy val messages = textOutput.messages
      def outputLabels = messages.filter(_.startsWith("label")).map(_.replace("label ", ""))
    }

    // print a label after each execution
    def simpleLabel(spec: Specification, output: Output) =
      label(spec)((i: Int) => {
        case f: Executable => printAfterExecution("label "+i, output)(f)
      })

    // print a label after each execution for examples and steps
    def exampleAndStepLabel(spec: Specification, output: Output) =
      label(spec)((i: Int) => {
        case f @ Example(_,_) => printAfterExecution("label example "+i, output)(f)
        case f @ Step(_,_)    => printAfterExecution("label step "+i, output)(f)
      })

    def printAfterExecution(s: String, output: Output) = (f: Executable) => {
      f.map { body =>
        val executed = body
        output.println(s)
        executed
      }.asInstanceOf[Fragment]
    }

    def label(spec: Specification)(f: Int => PartialFunction[Fragment, Fragment]) = {
      var i = 0
      new Specification { def is =
        spec.content.map { fragment =>
          i = i+1
          val function = f(i)
          if (function.isDefinedAt(fragment)) function.apply(fragment)
          else                                fragment
        }
      }
    }

  }


}