package org.specs2
package reporter
import io._
import specification._
import _root_.org.junit.runner._
import scalaz._
import Scalaz._

class JUnitDescriptionReporter(specificationKlass: Class[_]) extends Reporter with MockOutput {
  lazy val descriptionTree = new SpecificationTree[Description] {
	def map: Function[Fragment, Description] = {
	  case Text(t) => createSuiteDescription(testName(t))
      case Example(description, body) =>  createDescription(testName(description))
      case Step(action) => createDescription("specs2.silent")
      case other => createDescription("specs2.remove")
	}
  }
  override type T = (Map[Description, Fragment], descriptionTree.T)
  val initial = (Map.empty[Description, Fragment], descriptionTree.emptySpecTree)
  
  val folder = (descExamples: T, f: Fragment) => {
	val (examples, treeLoc) = descExamples
	val newTreeLoc = descriptionTree.folder(treeLoc, f)
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
  private def createDescription(s: String) = Description.createTestDescription(specificationKlass, sanitize(s))
  private def createSuiteDescription(s: String) = Description.createSuiteDescription(sanitize(s))
}

