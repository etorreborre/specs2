package org.specs2
package reporter

import _root_.org.junit.runner._
import scalaz._
import Scalaz._
import data.Trees._
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
 * The Description object creation works by using the Levels reducer to build a Tree[Description].
 * That Tree is then folded bottom-up to create the necessary associations between the 
 * Description objects. 
 * 
 */
class JUnitDescriptions(specificationClass: Class[_])  {
	import JUnitDescriptions._
	def foldAll(fs: Seq[Fragment]) = {
	  import Levels._
	  val descriptionTree = Levels.foldAll(fs).toTree(mapper)
	  DescriptionAndExamples(asOneDescription(descriptionTree), Map(descriptionTree.flatten:_*))
	}

}
object JUnitDescriptions {
  /**
   * This function is used to map each node in a Tree[Fragment] to a pair of 
   * (Description, Fragment)
   * 
   * The Int argument is the numeric label of the current TreeNode being mapped.
   * It is used to create a unique description of the example to executed which is required
   * by JUnit
   */
  val mapper: (Fragment, Int) => Option[(Description, Fragment)] = (f: Fragment, nodeLabel: Int) => f match {
    case (SpecStart(t, _))            => Some(createSuiteDescription(testName(t.name)) -> f)
    case (Text(t))                    => Some(createSuiteDescription(testName(t)) -> f)
    case (Example(description, body)) => Some(createDescription(testName(description.toString), nodeLabel) -> f)
    case (Step(action))               => Some(createDescription("step", nodeLabel) -> f)
    case other                        => None
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
  /** 
   * unfolding function attaching children descriptions to a parent one 
   * Note that:
   * * the Fragment in d: (Description, Fragment) is not used
   * * parent-child relations of the original tree are reworked to be JUnit-friendly and
   *   avoid to have an example being the ancestor of other examples like this:
   * 
   * text1
   * |
   * + ex1
   *   |
   *   ` text2
   *     |
   *     ` ex2
   *   
   * In that case the Description objects are arranged like this:
   * text1
   * |
   * + ex1
   * |
   * ` text2
   *   |
   *   ` ex2
   *    
   */
  private val addChildren = (d: (Description, Fragment), children: Stream[Description]) => {
    def isAnExample(child: Description) = child.getDisplayName.matches(".*\\(\\d*\\)$")
    def isText(child: Description) = !isAnExample(child)

    children.foreach { c =>
      if (!c.getChildren().isEmpty || !isText(c))
        d._1.addChild(c)
      if (!c.getChildren().isEmpty && isAnExample(c)) {
        c.getChildren().foreach(d._1.addChild(_))
        c.getChildren().clear()
      }
    }
    d._1
  }
  import text.Trim._
  /** @return a test name with no newlines */
  private def testName(s: String)= Trimmed(s).trimNewLines
  /** @return replace () with [] because it cause display issues in JUnit plugins */
  private def sanitize(s: String) = {
    val trimmed = Trimmed(s).trimReplace("(" -> "[",  ")" -> "]")
    if (trimmed.isEmpty) " "
    else trimmed
  }
  /** @return a test description */
  private def createDescription(s: String, e: Any) = Description.createSuiteDescription(sanitize(s)+"("+e.toString+")")
  /** @return a suite description */
  private def createSuiteDescription(s: String) = Description.createSuiteDescription(sanitize(s))
  
} 

