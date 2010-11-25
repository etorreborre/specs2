package org.specs2
package reporter

import _root_.org.junit.runner._
import scalaz._
import Scalaz._
import data.Trees._
import text.Trim._
import main.Arguments
import specification._

/**
 * The JUnit descriptions class transforms a list of fragments
 * to:
 * 
 * * a Description object having children Descriptions. It is used by the JUnitRunner
 *   to display the suites and tests to execute
 * * a Map of Fragments to execute, indexed by Description: Description -> Fragment 
 * 
 * The Description object creation works by using a TreeFold where each Fragment is mapped to a Description
 * to get a Tree[Description] that Tree is then folded bottom-up to create
 * the necessary associations between the Description objects.
 *
 */
class JUnitDescriptions(specificationClass: Class[_])  {
	import JUnitDescriptions._
	def foldAll(fs: Seq[Fragment]) = {
	  import LeveledBlocks._
	  val descriptionTree = LeveledBlocks.foldAll(fs).toTree(mapper)
	  DescriptionAndExamples(asOneDescription(descriptionTree), Map(descriptionTree.flatten:_*))
	}

}
object JUnitDescriptions {
  val mapper: (Fragment, Int) => Option[(Description, Fragment)] = (f: Fragment, nodeLabel: Int) => f match {
    case (SpecStart(t, _)) => 
      Some(createSuiteDescription(testName(t)) -> f)
    case (Text(t)) => 
      Some(createSuiteDescription(testName(t)) -> f)
    case (Example(description, body)) =>  Some(createDescription(testName(description), nodeLabel) -> f)
    case (Step(action)) => Some(createDescription("step", nodeLabel) -> f)
    case other => None
  }
  /** 
   * Utility class grouping the total description + fragments to execute for each Description 
   */
  case class DescriptionAndExamples(val description: Description, executions: Map[Description, Fragment])
  /**
   * @return a Description with parent-child relationships to other Description objects
   *         from a Tree[Description]
   */
  def asOneDescription(descriptionTree: Tree[(Description, Fragment)]): Description = {
    descriptionTree.bottomUp(addChildren).rootLabel
  }
  val addChildren = (d: (Description, Fragment), children: Stream[Description]) => { 
    children.foreach { c =>
      d._1.addChild(c) 
      if (!c.getChildren().isEmpty && c.getDisplayName().matches(".*\\(\\d*\\)")) {
        c.getChildren().foreach(d._1.addChild(_))
        c.getChildren().clear()
      }
    }
    d._1
  }
  /** @return a test name with no newlines */
  private def testName(s: String)= s.trimNewLines
  /** @return replace () with [] because it cause display issues in JUnit plugins */
  private def sanitize(s: String) = s.trimReplace("(" -> "[",  ")" -> "]")
  /** @return a test description */
  private def createDescription(s: String, e: Any) = 
    Description.createSuiteDescription(sanitize(s)+"("+e.toString+")")
  /** @return a suite description */
  private def createSuiteDescription(s: String) = Description.createSuiteDescription(sanitize(s))
  
} 

