package org.specs2.runner

import org.specs2.Specification
import org.specs2.specification._

object ConsoleReporterSpec extends ConsoleReporterSpecImplementation {
 val examples = 
"""
A console reporter is used to execute examples and display their status in the Console.

Examples are displayed with their description, status and a message if they're not ok.
Text fragments before examples are indented to give a visual indication
or more specific context:

  when a customer logs-in
    if he's a frequent customer
      + he must be greeted specially
      + if he must be presented his last orders
    if he's not a frequent customer
      + he must be presented the new products

The following examples specify the behavior for the display of a single example
and for the display of nested examples:
"""^
  "A single example must"^
    "have its description printed out" ~ e1^
    "be reported with a + if it is successful" ~ e2^
    "be reported with a x if it has a failure" ~ e3^
    "be reported with a x if it has a error" ~ e4^
    "be reported with a o if it is skipped or pending" ~ e5^
    "have the failure message displayed if it failed" ~ e6^
  br^  
  "Nested examples must be displayed as a tree"^
    "if a text starts a list of examples, there is one level" ~ e7^
    "if 2 text fragments start a list of examples, there are two levels" ~ e8^
    "if it is necessary to 'restart' the levels, ^^ must be used to separate the fragments" ~ e9
}

trait ConsoleReporterSpecImplementation extends Specification {
  def e1 = descriptionMustBe(1 must_== 1, "+ this example")
  def e2 = descriptionMustBe(1 must_== 1, "+ this example")
  def e3 = descriptionMustBe(1 must_== 2, "x this example")
  def e4 = descriptionMustBe({error("error"); 1 must_== 2}, "x this example")
  def e5 = descriptionMustBe(Pending("pending"), "o this example")
  def e6 = messageMustBe(1 must_== 2, "  '1' is not equal to '2'")
  
  val success = Success("ok")
  val failure = Failure("failure")
  val level1 = 
	"multi-level1.1"^
      "ex1" ~ success^
      "ex2" ~ success
  def e7 = reportIs(level1)(List(
    "multi-level1.1",
    "  + ex1",
    "  + ex2"))

  val level2 = 
  "examples are"^
    level1^
    "multi-level1.2"^
      "ex1" ~ failure^
      "ex2" ~ success
	  
  def e8 = reportIs(level2)(List(
    "examples are",
    "  multi-level1.1",
    "    + ex1",
    "    + ex2",
    "  multi-level1.2",
    "    x ex1",
    "      failure",
    "    + ex2"))

  val examplesWithResetToLevel0 =
  level2^^
  "an other example is"^
    "multi-level2.1"^
      "ex1" ~ success^
      "ex2" ~ success

  def e9 = reportIs(examplesWithResetToLevel0)(List(
    "examples are",
    "  multi-level1.1",
    "    + ex1",
    "    + ex2",
    "  multi-level1.2",
    "    x ex1",
    "      failure",
    "    + ex2",
    "an other example is",
    "  multi-level2.1",
    "    + ex1",
    "    + ex2"))
	
  def reportIs(examples: Examples)(output: List[String]) = {
	report(examples).mkString("\n", "\n", "\n") must_== output.mkString("\n", "\n", "\n") 
  }
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