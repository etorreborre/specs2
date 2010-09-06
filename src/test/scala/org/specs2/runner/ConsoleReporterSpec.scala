package org.specs2.runner

import org.specs2.Specification
import org.specs2.specification._

object ConsoleReporterSpec extends Specification {
 val examples = 
"""
A console reporter is used to execute examples and display their status in the Console.
"""^
  "A simple example must"^
    "have its description printed out" ~ e1^
    "be reported with a + if it is successful" ~ e2^
    "be reported with a x if it has a failure" ~ e3^
    "be reported with a x if it has a error" ~ e4^
    "be reported with a o if it is skipped or pending" ~ e5^
    "have the failure message displayed if it failed" ~ e6^
    ""
  def e1 = descriptionMustBe(1 must_== 1, "+ this example")
  def e2 = descriptionMustBe(1 must_== 1, "+ this example")
  def e3 = descriptionMustBe(1 must_== 2, "x this example")
  def e4 = descriptionMustBe({error("error"); 1 must_== 2}, "x this example")
  def e5 = descriptionMustBe(Pending("pending"), "o this example")
  def e6 = messageMustBe(1 must_== 2, "  '1' is not equal to '2'")
  
  def descriptionMustBe(body: =>Result, description: String) = {
	report("this example" ~ body).head must_== description 
  }
  def messageMustBe(body: Result, message: String) = {
	report("this example" ~ body)(1) must_== message 
  }
  def report(ex: Example): List[String] = report(Examples(List(ex))) 
  def report(ex: Examples): List[String] = {
	val reporter = new ConsoleReporter with MockOutput
	reporter.report(ex)
	reporter.messages.toList
 }
}