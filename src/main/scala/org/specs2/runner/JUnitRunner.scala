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
import junit.framework._
import specification._

class JUnitRunner(klass: Class[_]) extends Runner with ExampleExecution with ConsoleOutput {
  lazy val specification = tryToCreateObject[Specification](klass.getName, true, true).get
  lazy val (description, executions) = descriptionReporter.report(specification.examples.fragments)
  
  val descriptionReporter = new DescriptionReporter(klass.getSimpleName)
  
  def run(notifier: RunNotifier) {
	notifier.fireTestRunStarted(getDescription)
	executions.toStream.collect { case (desc, ex) => (desc, execute(ex)) }.
	  collect { 
		case (desc, ExecutedNoText()) => (desc, Success("specs2.silent")) 
		case (desc, ExecutedText(t)) => (desc, Success("specs2.text")) 
		case (desc, ExecutedResult(_, result)) => (desc, result) 
	  }.foreach { 
	 	case (desc, Success("specs2.silent")) => ()
	 	case (desc, result) => { 
	      if (result != Success("specs2.text")) 
	     	notifier.fireTestStarted(desc)
	      result match {
            case f @ Failure(m, st) => notifier.fireTestFailure(new notification.Failure(desc, junitFailure(f.exception)))
            case e @ Error(m, st) if desc.getDisplayName contains "specs2.silent" => { println(m); st foreach println }
            case e @ Error(m, st) => notifier.fireTestFailure(new notification.Failure(desc, e.exception))
            case Pending(_) => notifier.fireTestIgnored(desc) 
            case Skipped(_) => notifier.fireTestIgnored(desc) 
            case Success(_) => ()
          }
	      if (result != Success("specs2.text")) notifier.fireTestFinished(desc)
	    }
	  }	
  }
  def junitFailure(e: Exception): Throwable = new SpecFailureAssertionFailedError(e)
  def getDescription = description
}
class DescriptionReporter(specificationName: String) extends Reporter with MockOutput {
  type T = (Description, Map[Description, Fragment])
  val initial = (Description.createSuiteDescription(specificationName), Map.empty[Description, Fragment])
  val folder = (descAndExamples: (Description, Map[Description, Fragment]), f: Fragment) => {
	val (desc, examples) = descAndExamples
	f match {
      case Step(action) => (desc, examples + (createDescription("specs2.silent") -> f))
      case Text(t) => addDescription(desc, testName(t), f, examples)
      case ex @ Example(description, body) =>  addDescription(desc, testName(description), f, examples)
      case _ => (desc, examples)
	}
  }
  def testName(s: String)= {
	val spaces = s.takeWhile(_ == ' ')
	val name = (if (s contains "\n") (s.trim.split("\n")(0) + "...") else s.trim).replaceAll("\r", "")
	if (spaces.isEmpty)
      name
    else
      "." + spaces + name	  
  }
  def addDescription(desc: Description, d: String, f: Fragment, map: Map[Description, Fragment]) = {
	val exampleDesc = createDescription(d)
	desc.addChild(exampleDesc)
	(desc, map + (exampleDesc -> f)) 
  }
  def createDescription(s: String) = Description.createTestDescription(classOf[Specification], s)
}
/**
 * This class refines the <code>AssertionFailedError</code> from junit
 * and provides the stackTrace of an exception which occured during the specification execution
 */
class SpecFailureAssertionFailedError(e: Exception) extends AssertionFailedError(e.getMessage) {
  override def getStackTrace = e.getStackTrace
  override def getCause = e.getCause
  override def printStackTrace = e.printStackTrace
  override def printStackTrace(w: java.io.PrintStream) = e.printStackTrace(w)
  override def printStackTrace(w: java.io.PrintWriter) = e.printStackTrace(w)
}