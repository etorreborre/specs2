package org.specs2
package runner
import mock._
import specification._
import _root_.org.junit.runner.notification.{ RunNotifier, Failure }
import _root_.org.junit.runner.Description

class JUnitRunnerSpec extends SpecificationWithJUnit with Mockito with FragmentsSamples {
  val examples = 
"""	  
  The JUnitRunner is meant to be used with the RunWith annotation.
  It takes a Specification, execute it and notifies a RunNotifier of the possible failures.
  
  The following examples show the result of running a Specification with different successes or failures:
"""^
"  if the specification has 1 ok example, there must be a testStarted/testFinished notification" ! notified().e1^
"  if the specification has 2 ok examples, there must be a testStarted/testFinished notification for each" ! notified().e2^
"  if the specification has 1 failing example, there must be a testStarted/testFailure notification" ! notified().e3^
"  if the specification has 1 failing example, the failure message must be reported" ! notified().e4^
end

  case class notified() {
	val notifier = mock[RunNotifier]
	abstract class DummySpec extends Specification 
	def run(f: Fragments) = JUnitRunner[DummySpec](f).run(notifier)
	def desc(s: String) = Description.createTestDescription(classOf[DummySpec], s)
	
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
  }
  
}