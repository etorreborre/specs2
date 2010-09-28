package org.specs2
package reporter
import io._
import specification._
import _root_.org.junit.runner._
import scalaz._
import Scalaz._

class JUnitDescriptionReporter(specificationName: String) extends Reporter with MockOutput {
  lazy val descriptionTree = new SpecificationTree[Description] {
	def map: Function[Fragment, Description] = {
	  case Text(t) => createDescription(testName(t))
      case Example(description, body) =>  createDescription(testName(description))
      case other => createDescription(other.toString)
	}
  }
  override type T = (Map[Description, Fragment], descriptionTree.T)
  val initial = (Map.empty[Description, Fragment], descriptionTree.emptySpecTree)
  
  val folder = (descExamples: T, f: Fragment) => {
	val (examples, treeLoc) = descExamples
	val newTreeLoc = descriptionTree.folder(treeLoc, f)
	val newExamples = f match {
      case Step(action) => examples + (createDescription("specs2.silent") -> f)
      case Text(t) => examples + (createDescription(testName(t)) -> f)
      case Example(description, body) =>  examples + (createDescription(testName(description)) -> f)
      case _ => examples
	}
	(newExamples, newTreeLoc)
  }
  
  def testName(s: String)= {
	val spaces = s.takeWhile(_ == ' ')
	val name = (if (s contains "\n") (s.trim.split("\n")(0) + "...") else s.trim).replaceAll("\r", "")
	if (spaces.isEmpty)
      name
    else
      "." + spaces + name	  
  }
  def createDescription(s: String) = Description.createTestDescription(classOf[Specification], s)
}
