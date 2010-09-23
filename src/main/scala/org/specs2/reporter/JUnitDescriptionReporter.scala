package org.specs2
package reporter
import io._
import specification._
import _root_.org.junit.runner._

class JUnitDescriptionReporter(specificationName: String) extends Reporter with NestedLevels with MockOutput {
  type T = (Description, Map[Description, Fragment], Level)
  val initial = (Description.createSuiteDescription(specificationName), Map.empty[Description, Fragment], Level())
  
  val folder = (descExamplesAndLevel: (Description, Map[Description, Fragment], Level), f: Fragment) => {
	val (desc, examples, level) = descExamplesAndLevel
	f match {
      case Step(action) => (desc, examples + (createDescription("specs2.silent") -> f), level)
      case Text(t) => {
    	val description = addDescription(desc, testName(t))
    	if (level.state == Down)
    	  (description, examples + (description -> f), level)
    	else
    	  (desc, examples + (description -> f), level)
      }
      case ex @ Example(description, body) =>  (desc, examples + (addDescription(desc, testName(description)) -> f), level)
      case _ => (desc, examples, level)
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
  def addDescription(desc: Description, d: String) = {
	val exampleDesc = createDescription(d)
	desc.addChild(exampleDesc)
	exampleDesc
  }
  def createDescription(s: String) = Description.createTestDescription(classOf[Specification], s)
}
