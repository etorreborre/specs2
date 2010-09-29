package org.specs2
package runner
import reporter._
import execute._
import specification._
import io._
import reflect.Classes._
import specification._
import reporter._
import _root_.org.junit.runner.notification.RunNotifier
import _root_.org.junit.runners._
import _root_.org.junit._
import junit.framework._
import _root_.org.junit.runner._

class JUnitRunner(klass: Class[_]) extends Runner with ExampleExecution with ConsoleOutput {
  val descriptionReporter = new JUnitDescriptionReporter(klass)

  lazy val specification = tryToCreateObject[Specification](klass.getName, true, true).get
  lazy val (executions, (descriptionTree, level)) = descriptionReporter.report(specification.examples.fragments)
  
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
  private def junitFailure(e: Exception): Throwable = new SpecFailureAssertionFailedError(e)
  def getDescription = treeDescription
  
  private lazy val treeDescription = {
    import scalaz.Tree
    def addDescriptions(tree: Tree[Description]): Description = {
      tree.subForest.foreach(sub => tree.rootLabel.addChild(addDescriptions(sub)))	
      tree.rootLabel
    }
    addDescriptions(descriptionTree.toTree)
  }
}
trait ShowDescription {
  implicit object show extends scalaz.Show[Description] {
    def show(d: Description) = d.getDisplayName.toList
  }
}
object ShowDescription extends ShowDescription
/**
 * This class refines the <code>AssertionFailedError</code> from junit
 * and provides the stackTrace of an exception which occurred during the specification execution
 */
class SpecFailureAssertionFailedError(e: Exception) extends AssertionFailedError(e.getMessage) {
  override def getStackTrace = e.getStackTrace
  override def getCause = e.getCause
  override def printStackTrace = e.printStackTrace
  override def printStackTrace(w: java.io.PrintStream) = e.printStackTrace(w)
  override def printStackTrace(w: java.io.PrintWriter) = e.printStackTrace(w)
}