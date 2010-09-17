package org.specs2
package runner
import reporter._
import execute._
import specification._
import io._
import reflect.Classes._
import org.junit.runner._
import org.junit.runner.notification.RunNotifier
import org.junit.runners._
import org.junit._

class JUnitRunner(klass: Class[_]) extends Runner with ExampleExecution with MockOutput {
  lazy val specification = tryToCreateObject[Specification](klass.getName, true, true).get
  lazy val (description, executions) = descriptionReporter.report(specification.examples.fragments)
  
  val descriptionReporter = new DescriptionReporter(klass.getSimpleName)
  
  def run(notifier: RunNotifier) {
	val result = new org.junit.runner.Result
	notifier.addFirstListener(result.createListener)
	notifier.fireTestRunStarted(getDescription)
	executions.toStream.collect { case (desc, ex) => (desc, execute(ex)) }.
	  collect { case (desc, ExecutedResult(_, result)) => (desc, result) }.
	  foreach { case (desc, result) => 
	    notifier.fireTestStarted(desc)
	    result match {
          case f @ Failure(m, st) => notifier.fireTestFailure(new notification.Failure(desc, f.exception))
          case e @ Error(m, st) => notifier.fireTestFailure(new notification.Failure(desc, e.exception))
          case Pending(_) => notifier.fireTestIgnored(desc) 
          case Skipped(_) => notifier.fireTestIgnored(desc) 
          case Success(_) => ()
        }
	    notifier.fireTestFinished(desc)
	  }
	notifier.fireTestRunFinished(result)
  }
  def getDescription = description
}
class DescriptionReporter(specificationName: String) extends Reporter with MockOutput {
  type T = (Description, Map[Description, Example])
  val initial = (Description.createSuiteDescription(specificationName), Map.empty[Description, Example])
  val folder = (descAndExamples: (Description, Map[Description, Example]), f: Fragment) => {
	val (desc, examples) = descAndExamples
	f match {
      case ex @ Example(description, body) => {
    	val exampleDesc = Description.createTestDescription(classOf[Specification], description)
    	desc.addChild(exampleDesc)
    	(desc, examples + (exampleDesc -> ex)) 
      }
      case _ => (desc, examples)
	}
  }
}
