package org.specs2
package reporter
import control.LazyParameters._
import text.AnsiColors._
import main._
import io._
import execute._
import matcher._
import specification._

class ConsoleReporterSpec extends ConsoleReporterSpecImplementation { def is = 
                                                                                          """
A console reporter is used to execute Fragments and display their status in the Console.

Fragments are displayed with their description, status and a message if they're not ok.

Text fragments before Fragments are indented to give a visual indication
of a more specific context (but context management is done independently), like this:

  when a customer logs-in
    if he's a frequent customer
      + he must be greeted specially
      + if he must be presented his last orders
    if he's not a frequent customer
      + he must be presented the new products

The following Fragments specify the behavior for:

  * the display of a single example
  * the display of nested Fragments
  * the display of statistics at the end
                                                                                          """^
                                                                                          p^
  "A single example must"                                                                 ^
    "have its description printed out"                                                    ! single1^
    "be reported with a + if it is successful"                                            ! single2^
    "be reported with a x if it has a failure"                                            ! single3^
    "be reported with a ! if it has a error"                                              ! single4^
    "be reported with a * if it is pending"                                               ! single5^
    "be reported with a o if it is skipped "                                              ! single5_1^
    "have the failure message displayed if it failed"                                     ! single6^
    "have the file location displayed if it is a failure or an error"                     ! single7^
                                                                                          p^  
  "At the end of the report"                                                              ^
    "the total number of Fragments must be displayed"                                     ! stat1^
    "the total number of failures must be displayed"                                      ! stat2^
    "the total number of expectations must be displayed " +                               
    "if it is different from the number of examples"                                      ! stat3^
                                                                                          end
}

abstract class ConsoleReporterSpecImplementation extends SpecificationWithJUnit with FragmentsSamples with ExpectedOutputs with ReportExpectations {
  def single1 = descriptionMustBe(1 must_== 1, "+ this example")
  def single2 = descriptionMustBe(1 must_== 1, "+ this example")
  def single3 = descriptionMustBe(1 must_== 2, "x this example")
  def single4 = descriptionMustBe({error("error"); 1 must_== 2}, "! this example")
  def single5 = descriptionMustBe(Pending("PENDING"), "* this example PENDING")
  def single5_1 = descriptionMustBe(Skipped("not ready"), "o this example")
  def single6 = messagesContain(1 must_== 2, "'1' is not equal to '2'")
  def single7 = messagesContain(1 must_== 2, "ConsoleReporterSpec.scala")
  
  def stat1 = reportEndsWith(level1 ^ SpecEnd(""))(level1Stats)
  def stat2 = reportEndsWith(level2WithFailure ^ SpecEnd(""))(level2WithFailureStats)
  def stat3 = reportEndsWith("level1" ^ exampleWithExpectations ^ SpecEnd(""))(exampleWithExpectationsStats)
}
trait ReportExpectations extends MustExpectations with FragmentsBuilder with Matchers {
  def reportStartsWith(Fragments: Fragments)(output: List[String]) = {
	  report(Fragments).mkString("\n", "\n", "\n") must startWith(output.mkString("\n", "\n", "\n"))
  }
  def reportEndsWith(Fragments: Fragments)(output: List[String]) = {
	  report(Fragments).mkString("\n", "\n", "\n") must endWith(output.mkString("\n", "\n", "\n"))
  }
  def reportIs(Fragments: Fragments)(output: List[String]) = {
	  report(Fragments).mkString("\n", "\n", "\n") must_== output.mkString("\n", "\n", "\n") 
  }
  def descriptionMustBe(body: =>Result, description: String) = {
	  report("this example" ! body).head must_== description 
  }
  def messageMustBe(body: Result, message: String) = {
	  report("this example" ! body)(1) must_== message 
  }
  def messagesContain(body: Result, message: String) = {
	  report("this example" ! body) must containMatch(message) 
  }
  def report(ex: Example): List[String] = report(Fragments(ex)) 
  def report(ex: Fragments): List[String] = {

  val textOutput = new TextResultOutput with MockOutput
  trait MockTextPrinter extends TextPrinter { override val output = textOutput } 
	val reporter = new ConsoleReporter with MockTextPrinter
	  reporter.report(getClass, ex)(Arguments())
	  textOutput.messages.toList.map(removeColors(_))
  }
}
trait ExpectedOutputs {

  val level1Stats = List(
    "Total for specification",
    "2 examples, 0 failure, 0 error",
    " ")
    
  val level2WithFailureStats = List(
	  "Total for specification",
    "2 examples, 1 failure, 0 error",
    " ")
  val exampleWithExpectationsStats = List(
    "Total for specification",
    "1 example, 2 expectations, 0 failure, 0 error",
    " ")}