package org.specs2
package reporter

import _root_.org.junit.runner._
import org.specs2.data.Trees._
import org.specs2.internal.scalaz._
import org.specs2.internal.scalaz.Scalaz._
import org.specs2.internal.scalaz.Traverse._
import main.Arguments
import specification._
import control.{ExecutionOrigin, Stacktraces}
import text.Trim.Trimmed._
import java.util.regex.Matcher

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
  import JUnitDescriptionMaker._
  type DescribedFragment = (Description, Fragment)
  def foldAll(fs: Seq[Fragment])(implicit args: Arguments) = {
    import Levels._
    val leveledFragments = Levels.foldAll(select(fs))
    lazy val root = createDescription(specificationClass, specificationClass.getName)
    implicit val initial: DescribedFragment = (root, Text(specificationClass.getName))

    if (leveledFragments.isEmpty) DescriptionAndExamples(root, Seq(initial).toStream)
    else {
      val descriptionTree = leveledFragments.toTree(mapper(specificationClass))
      val removeDanglingText = (t: Tree[DescribedFragment]) => {
        t.rootLabel  match {
          case (desc, Text(_)) if t.subForest.isEmpty  => (None:Option[DescribedFragment])
          case other                                   => Some(t.rootLabel)
        }
      }
      val prunedDescriptionTree = descriptionTree.prune(removeDanglingText)
      DescriptionAndExamples(asOneDescription(prunedDescriptionTree), prunedDescriptionTree.flatten)
    }
  }

}
private[specs2]
trait JUnitDescriptionMaker extends ExecutionOrigin {
  type DescribedFragment = (Description, Fragment)
  /**
   * This function is used to map each node in a Tree[Fragment] to a pair of 
   * (Description, Fragment)
   * 
   * The Int argument is the numeric label of the current TreeNode being mapped.
   * It is used to create a unique description of the example to executed which is required
   * by JUnit
   */
  def mapper(klass: Class[_]): (Fragment, Seq[DescribedFragment], Int) => Option[DescribedFragment] =
    (f: Fragment, parentNodes: Seq[DescribedFragment], nodeLabel: Int) => f match {
      case (SpecStart(t, _))            => Some(createDescription(klass, suiteName=testName(t.name)) -> f)
      case (Text(t))                    => Some(createDescription(klass, suiteName=testName(t)) -> f)
      case (Example(description, body)) => Some(createDescription(klass, label=nodeLabel.toString, testName=testName(description.toString, parentPath(parentNodes))) -> f)
      case (Step(action))               => Some(createDescription(klass, label=nodeLabel.toString, testName="step") -> f)
      case (Action(action))             => Some(createDescription(klass, label=nodeLabel.toString, testName="action") -> f)
      case other                        => None
    }
  /**
   * Utility class grouping the total description + fragments to execute for each Description 
   */
  case class DescriptionAndExamples(val description: Description, executions: Stream[DescribedFragment])
  /**
   * @return a Description with parent-child relationships to other Description objects
   *         from a Tree[Description]
   */
  def asOneDescription(descriptionTree: Tree[DescribedFragment])(implicit args: Arguments = Arguments()): Description = {
    if (args.noindent)
      descriptionTree.flatten.drop(1).foldLeft(descriptionTree.rootLabel)(flattenChildren)._1
    else
      descriptionTree.bottomUp(addChildren).rootLabel._1
  }
  /** 
   * unfolding function attaching children descriptions their parent
   */
  private val addChildren = (desc: (Description, Fragment), children: Stream[DescribedFragment]) => {
    children.foreach { child => desc._1.addChild(child._1) }
    desc
  }
  /**
   * unfolding function attaching children descriptions the root
   */
  private def flattenChildren = (result: DescribedFragment, current: DescribedFragment) => {
    result._1.addChild(current._1)
    result
  }
  /** @return a sanitized description */
  def createDescription(testClass: Class[_], suiteName: String = "", testName: String = "", label: String = "") = {
    val origin =
      if (isExecutedFromAnIDE && !label.isEmpty) label
      else testClass.getName

    val desc=
      if (testName.isEmpty) (if (suiteName.isEmpty) testClass.getSimpleName else suiteName)
      else sanitize(testName)+"("+origin+")"
    Description.createSuiteDescription(desc)
  }

  import text.Trim._

  /** @return a seq containing the path of an example without the root name */
  private def parentPath(parentNodes: Seq[DescribedFragment]) = parentNodes.drop(1).map(_._1.getDisplayName)

  /** @return a test name with no newlines */
  private def testName(s: String, parentNodes: Seq[String] = Seq()): String = {
    (if (parentNodes.isEmpty || isExecutedFromAnIDE) "" else parentNodes.mkString("", "::", "::")) +
    Trimmed(s).trimNewLines
  }


  /** @return replace () with [] because it cause display issues in JUnit plugins */
  private def sanitize(s: String) = {
    val trimmed = Trimmed(s).trimReplace("(" -> "[",  ")" -> "]")
    if (trimmed.isEmpty) " "
    else trimmed
  }
}
private[specs2]
object JUnitDescriptionMaker extends JUnitDescriptionMaker
