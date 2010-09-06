package org.specs2.runner

import org.specs2.Specification
import org.specs2.specification._

object ConsoleReporterSpec extends Specification {
 val examples = 
"""
A console reporter is used to execute examples and display their status in the Console.
"""^
  "A simple example"^
    "must have its description printed out" ~ e1
    
  def e1 = report("this example" ~ (1 must_== 1)) must_== List("this example")
  def report(ex: Example): List[String] = report(Examples(List(ex))) 
  def report(ex: Examples): List[String] = {
	val reporter = new ConsoleReporter with MockOutput
	reporter.report(ex)
	reporter.messages.toList
 }
}