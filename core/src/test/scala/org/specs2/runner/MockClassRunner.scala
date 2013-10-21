package org.specs2.runner

import org.specs2.main.Arguments
import org.specs2.specification.SpecificationStructure
import org.specs2.reporter.{TextResultOutput, ConsoleReporter}
import org.specs2.io.StringOutput

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


