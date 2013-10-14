package org.specs2
package runner

import specification.SpecificationStructure
import reporter.{TextResultOutput, ConsoleReporter}
import io.StringOutput
import main.Arguments
import Arguments._

case class MockClassRunner(arguments: Arguments = Arguments()) extends ClassRunner {
  def run(s: SpecificationStructure) = {
    val reporter = newReporter
    reporter.report(s)(arguments)
    reporter.textOutput.messages.map(arguments.removeColors)
  }

  def newReporter = new ConsoleReporter {
    override lazy val textOutput = new TextResultOutput with StringOutput
    def messages = textOutput.messages
  }

}


