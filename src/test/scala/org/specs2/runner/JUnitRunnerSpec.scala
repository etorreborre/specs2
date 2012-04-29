package org.specs2
package runner
import mock._
import specification._
import reporter._
import main._
import _root_.org.junit.runner.notification.{ RunNotifier, Failure }
import _root_.org.junit.runner.Description
import _root_.org.junit.ComparisonFailure
import text.FromString

class JUnitRunnerSpec extends Specification with Mockito with FragmentsSamples {  def is =
  
                                                                                                                        """
  The JUnitRunner is meant to be used with the RunWith annotation.
  It takes a Specification, executes it and notifies a RunNotifier object of the possible
  failures.

  The following examples show the result of running a Specification with different
  successes or failures:
                                                                                                                        """^
                                                                                                                        p^
  "If the specification has"                                                                                            ^
    "1 ok example, there must be a testStarted/testFinished notification"                                               ! notified().e1^
    "2 ok examples, there must be a testStarted/testFinished notification for each"                                     ! notified().e2^
    "1 failing example, there must be a testStarted/testFailure notification"                                           ! notified().e3^
    "1 failing example, the failure message must be reported"                                                           ! notified().e4^
    "1 example with an error, the error message must be reported"                                                       ! notified().e5^
    "1 skipped example, a test ignored must be reported"                                                                ! notified().e6^
    "1 pending example, a test ignored must be reported"                                                                ! notified().e7^
    "1 failing example with be_==, a ComparisonFailure message must be reported"                                        ! notified().e8^
                                                                                                                        p^
  "If the console system property is specified"                                                                         ^
    "then the specification is also printed on the console"                                                             ! export().e1^
    "the commandline system property can be used to remove colors"                                                      ! export().e2^
                                                                                                                        p^
  "If an exclude tag is passed as a system property"                                                                    ^
    "then the corresponding examples must be excluded"                                                                  ! excluded().e1^
                                                                                                                        p^
  "If the isolated argument is specified"                                                                               ^
    "then the examples executions must be isolated"                                                                     ! isolate().e1^
                                                                                                                        end

  trait WithNotifier {
    lazy val notifier   = mock[RunNotifier]
    lazy val console    = mock[TextExporting]
    lazy val html       = mock[HtmlExporting]
    implicit lazy val properties = mock[SystemProperties]

    properties.getProperty(anyString) returns None
    properties.getPropertyAs[Boolean](anyString)(any[FromString[Boolean]]) returns None

    val executeSpec = (s: ExecutingSpecification) => s.execute
    Seq(console, html).foreach(e => e.export(any[Arguments]) returns executeSpec)

    abstract class DummySpecification extends Specification
    def run(f: Fragments) = JUnitRunner.apply[DummySpecification](f, properties, console, html).run(notifier)
    def run(spec: SpecificationStructure) = JUnitRunner.apply(spec).run(notifier)
  }

  case class notified() extends WithNotifier {
	  def desc(s: String) = =~(s) ^^ ((_:Description).getDisplayName)
	  
	  def e1 = { 
	    run(ex1)
	    Seq(there was one(notifier).fireTestStarted(desc("ex1")),
	        there was one(notifier).fireTestFinished(desc("ex1")))
    }
	  def e2 = { 
	    run(level1)
	    Seq("ex1", "ex2") flatMap { s =>
	      Seq(there was one(notifier).fireTestStarted(desc(s)),
	          there was one(notifier).fireTestFinished(desc(s)))
	    }
    }
	  def e3 = { 
	    run(ex1Failure)
	    Seq(there was one(notifier).fireTestStarted(desc("ex1")),
	        there was one(notifier).fireTestFailure(any[Failure]))
    }
	  def e4 = { 
	    run(ex1Failure)
	    val c = capture[Failure]
	    there was one(notifier).fireTestFailure(c)
	    c.value.getMessage must_== "failure"
    }
	  def e5 = { 
	    run(ex1Error)
	    there was one(notifier).fireTestFailure(be_==("error")^^((_:Failure).getMessage))
    }
	  def e6 = { 
	    run(ex1Skipped)
	    there was one(notifier).fireTestIgnored(desc("ex1"))
    }
	  def e7 = { 
	    run(ex1Pending)
	    there was one(notifier).fireTestIgnored(desc("ex1"))
    }
    def e8 = {
      run(ex1BeEqualToFailure)
      val c = capture[Failure]
      there was one(notifier).fireTestFailure(c)
      c.value.getException must haveSuperclass[ComparisonFailure]
    }
  }
  case class export() extends WithNotifier {

    def e1 = {
      properties.isDefined("console") returns true
      console.export(any[Arguments]) returns ((spec: ExecutingSpecification) => spec.executed)

      run(ex1)
      there was one(console).export(any[Arguments])
    }
    def e2 = {
      properties.isDefined("html") returns true
      html.export(any[Arguments]) returns ((spec: ExecutingSpecification) => spec.executed)

      run(ex1)
      there was one(html).export(any[Arguments])
    }
  }
  case class excluded() extends WithNotifier {

    def e1 = {
      properties.getProperty("exclude") returns Some("wontwork")
      run(new TaggedSpecWithOneExcludedExample)
      there was no(notifier).fireTestFailure(any[Failure])
    }
  }
  case class isolate() extends WithNotifier {

    def e1 = {
      run(new examples.HelloWorldUnitIsolatedSpec)
      there was no(notifier).fireTestFailure(any[Failure])
    }
  }
}

class TaggedSpecWithOneExcludedExample extends mutable.SpecificationWithJUnit with mutable.Tags {
  "this example is ok" >> ok

  tag("wontwork")
  "this example is not" >> ko
}