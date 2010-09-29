package org.specs2
package reporter
import io._
import specification._
import _root_.org.junit.runner._
import scalaz._
import Scalaz._

class JUnitDescriptionFold(specificationClass: Class[_]) extends Fold {
	
  object descriptionTree extends TreeFold[Description] {
	def root = createSuiteDescription(specificationClass.getSimpleName)
	def map: Function[Fragment, Option[Description]] = {
	  case Text(t) => Some(createSuiteDescription(testName(t)))
      case Example(description, body) =>  Some(createDescription(testName(description)))
      case Step(action) => Some(createDescription("specs2.silent"))
      case other => None
	}
  }
  
  override type T = (Map[Description, Fragment], descriptionTree.T)
  val initial = (Map.empty[Description, Fragment], descriptionTree.initial)
  
  val fold = (descExamples: T, f: Fragment) => {
	val (examples, treeLoc) = descExamples
	val newTreeLoc = descriptionTree.fold(treeLoc, f)
	val newExamples = f match {
      case Step(action) => examples + (createDescription("specs2.silent") -> f)
      case Text(t) => examples + (createSuiteDescription(testName(t)) -> f)
      case Example(description, body) =>  examples + (createDescription(testName(description)) -> f)
      case _ => examples
	}
	(newExamples, newTreeLoc)
  }
  
  def testName(s: String)= {
	(if (s contains "\n") (s.trim.split("\n")(0) + "...") else s.trim).replaceAll("\r", "")
  }
  private def sanitize(s: String) = s.replace("(", "[").replace(")", "]")
  private def createDescription(s: String) = Description.createTestDescription(specificationClass, sanitize(s))
  private def createSuiteDescription(s: String) = Description.createSuiteDescription(sanitize(s))
}

