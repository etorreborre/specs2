package org.specs2
package runner
import mock._
import specification._
import _root_.org.junit.runner.notification.{ RunNotifier, Failure }
import _root_.org.junit.runner.Description

class JUnitRunnerSpec extends SpecificationWithJUnit with Mockito with FragmentsSamples {  def is =
  
                                                                                          """	  
  The JUnitRunner is meant to be used with the RunWith annotation.
  It takes a Specification, execute it and notifies a RunNotifier object of the possible 
  failures.
  
  The following examples show the result of running a Specification with different 
  successes or failures:
                                                                                          """                                                                                                        ^
"  if the specification has 1 ok example, there must be a testStarted/testFinished"       +
"  notification"                                                                          ! notified().e1^
"  if the specification has 2 ok examples, there must be a testStarted/testFinished"      +
"  notification for each"                                                                 ! notified().e2^
"  if the specification has 1 failing example, there must be a testStarted/testFailure"   +
"   notification"                                                                         ! notified().e3^
"  if the specification has 1 failing example, the failure message must be reported"      ! notified().e4^
"  if the specification has 1 example with an error, the error message must be reported"  ! notified().e5^
"  if the specification has 1 skipped example, a test ignored must be reported"           ! notified().e6^
"  if the specification has 1 pending example, a test ignored must be reported"           ! notified().e7^
                                                                                          end

  case class notified() {
	  val notifier = mock[RunNotifier]
	  abstract class DummySpec extends Specification 
	  def run(f: Fragments) = JUnitRunner[DummySpec](f).run(notifier)
	  def desc(s: String) = Description.createSuiteDescription(s)
	  
	  def e1 = { 
	    run(ex1)
	    Seq(there was one(notifier).fireTestStarted(desc("ex1(1)")),
	        there was one(notifier).fireTestFinished(desc("ex1(1)")))
    }
	  def e2 = { 
	    run(level1)
	    Seq("ex1(2)", "ex2(3)") flatMap { s =>
	      Seq(there was one(notifier).fireTestStarted(desc(s)),
	          there was one(notifier).fireTestFinished(desc(s)))
	    }
    }
	  def e3 = { 
	    run(ex1Failure)
	    Seq(there was one(notifier).fireTestStarted(desc("ex1(1)")),
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
	    there was one(notifier).fireTestIgnored(desc("ex1(1)"))
    }
	  def e7 = { 
	    run(ex1Pending)
	    there was one(notifier).fireTestIgnored(desc("ex1(1)"))
    }
  }
  
}