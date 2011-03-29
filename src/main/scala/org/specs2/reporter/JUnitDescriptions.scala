package org.specs2
package reporter

import _root_.org.junit.runner._
import scalaz._
import Scalaz._
import data.Trees._
import main.Arguments
import specification._
import control.{ExecutionOrigin, Stacktraces}

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
private[specs2]
class JUnitDescriptions(specificationClass: Class[_]) extends DefaultSelection {
  import JUnitDescriptions._
  def foldAll(fs: Seq[Fragment]) = {
    import Levels._
    val leveledFragments = Levels.foldAll(select(fs))
    if (leveledFragments.isEmpty) {
      val root = createDescription(specificationClass.getName, klassName=specificationClass.getName)
      DescriptionAndExamples(root, Seq((root, Text(specificationClass.getName))).toStream)
    }
    else {
      val descriptionTree = leveledFragments.toTree(mapper(specificationClass.getName))
      DescriptionAndExamples(asOneDescription(descriptionTree), descriptionTree.flatten)
    }
  }

}
private[specs2]
object JUnitDescriptions extends ExecutionOrigin {
  /**
   * This function is used to map each node in a Tree[Fragment] to a pair of 
   * (Description, Fragment)
   * 
   * The Int argument is the numeric label of the current TreeNode being mapped.
   * It is used to create a unique description of the example to executed which is required
   * by JUnit
   */
  def mapper(className: String): (Fragment, Int) => Option[(Description, Fragment)] = (f: Fragment, nodeLabel: Int) => f match {
    case (SpecStart(t, _))            => Some(createDescription(testName(t.name), klassName=className) -> f)
    case (Text(t))                    => Some(createDescription(testName(t), klassName=className) -> f)
    case (Example(description, body)) => Some(createDescription(testName(description.toString), nodeLabel.toString, className) -> f)
    case (Step(action))               => Some(createDescription("step", nodeLabel.toString, className) -> f)
    case (Action(action))             => Some(createDescription("action", nodeLabel.toString, className) -> f)
    case other                        => None
  }
  /**
   * Utility class grouping the total description + fragments to execute for each Description 
   */
  case class DescriptionAndExamples(val description: Description, executions: Stream[(Description, Fragment)])
  /**
   * @return a Description with parent-child relationships to other Description objects
   *         from a Tree[Description]
   */
  def asOneDescription(descriptionTree: Tree[(Description, Fragment)]): Description = {
    descriptionTree.bottomUp(addChildren).rootLabel
  }
  /** 
   * unfolding function attaching children descriptions to a parent one 
   */
  private val addChildren = (desc: (Description, Fragment), children: Stream[Description]) => {
    children.foreach { child => desc._1.addChild(child) }
    desc._1
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
  /** @return a sanitized description */
  def createDescription(s: String, label: String= "", klassName: String="") = {
    val code =
      if ((isExecutedFromAnIDE || isExecutedFromSBT) && !label.isEmpty)
        "("+label+")"
      else if (isExecutedFromGradle && !klassName.isEmpty)
        "("+klassName+")"
      else ""
    Description.createSuiteDescription(sanitize(s)+code)
  }
  
}
