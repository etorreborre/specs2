package org.specs2
package reporter

import _root_.org.junit.runner._
import org.specs2.data.Trees._
import scalaz._
import scalaz.Scalaz._
import scalaz.Traverse._
import main.Arguments
import specification._
import control.{ExecutionOrigin, Stacktraces}

/**
 * The JUnit descriptions class transforms a list of fragments
 * to:
 * 
 * - a Description object having children Descriptions. It is used by the JUnitRunner
 *   to display the suites and tests to execute
 * - a Map of Fragments to execute, or executed fragments, indexed by Description: Description -> Fragment
 * 
 * The Description object creation works by using the Levels reducer to build a Tree[Description].
 * That Tree is then folded bottom-up to create the necessary associations between the 
 * Description objects. 
 * 
 */
private[specs2]
abstract class JUnitDescriptions[F](className: String)(implicit reducer: Reducer[F, Levels[F]]) extends JUnitDescriptionMaker[F] {
  import Levels._

  /**
   * @return fragment that must be used as the root description if the specification is empty
   */
  def initialFragment(className: String): F

  private implicit lazy val initial = initialFragment(className) -> specificationDescription
  private lazy val defaultDescription = (f: F) => Description.createSuiteDescription("fragment not found:" + f)

  def foldAll(fs: Seq[F])(implicit args: Arguments) = {
    val leveledFragments = Levels.foldAll(fs)(reducer)

    if (leveledFragments.isEmpty)
      DescriptionAndExamples(specificationDescription, Map(initial).withDefault(defaultDescription))
    else {
      val descriptionTree = leveledFragments.toTree[DescribedFragment](mapper(className))
      val removeDanglingText = (t: Tree[DescribedFragment]) => {
        t.rootLabel  match {
          case (txt: Text, desc) if t.subForest.isEmpty  => (None:Option[DescribedFragment])
          case other                                     => Some(t.rootLabel)
        }
      }
      val prunedDescriptionTree = descriptionTree.prune(removeDanglingText)
      DescriptionAndExamples(asOneDescription(prunedDescriptionTree), Map(prunedDescriptionTree.flatten.toSeq:_*).withDefault(defaultDescription))
    }
  }

  lazy val specificationDescription = createDescription(className, className)
}

private[specs2]
trait JUnitDescriptionMaker[F] extends ExecutionOrigin {
  type DescribedFragment = (F, Description)
  /**
   * This function is used to map each node in a Tree[Fragment] to a pair of 
   * (Description, Fragment)
   * 
   * The Int argument is the numeric label of the current TreeNode being mapped.
   * It is used to create a unique description of the example to executed which is required
   * by JUnit
   */
  def mapper(className: String): (F, Seq[DescribedFragment], Int) => Option[DescribedFragment]
  /**
   * @return a Description with parent-child relationships to other Description objects
   *         from a Tree[Description]
   */
  def asOneDescription(descriptionTree: Tree[DescribedFragment])(implicit args: Arguments = Arguments()): Description = {
    descriptionTree.bottomUp(addChildren).rootLabel._2
  }
  /** 
   * unfolding function attaching children descriptions their parent
   */
  private val addChildren = (desc: (F, Description), children: Stream[DescribedFragment]) => {
    children.foreach { child => desc._2.addChild(child._2) }
    desc
  }
  /**
   * unfolding function attaching children descriptions the root
   */
  private def flattenChildren = (result: DescribedFragment, current: DescribedFragment) => {
    result._2.addChild(current._2)
    result
  }
  /** @return a sanitized description */
  def createDescription(className: String, suiteName: String = "", testName: String = "", label: String = "") = {
    val origin =
      if (isExecutedFromAnIDE && !label.isEmpty) label
      else className

    val desc=
      if (testName.isEmpty) (if (suiteName.isEmpty) className else suiteName)
      else sanitize(testName)+"("+origin+")"
    
    Description.createSuiteDescription(desc)
  }

  import text.Trim._

  /** @return a seq containing the path of an example without the root name */
  def parentPath(parentNodes: Seq[DescribedFragment]) = Vector(parentNodes.drop(1).map(_._2.getDisplayName):_*)

  /** @return a test name with no newlines */
  def testName(s: String, parentNodes: Seq[String] = Seq()): String = {
    (if (parentNodes.isEmpty || isExecutedFromAnIDE) "" else parentNodes.map(_.replace("\n", "")).mkString("", "::", "::")) +
    (if (isExecutedFromAnIDE) Trimmed(s).removeNewLines else Trimmed(s).trimNewLines)
  }


  /** @return replace () with [] because it cause display issues in JUnit plugins */
  private def sanitize(s: String) = {
    val trimmed = Trimmed(s).trimReplace("(" -> "[",  ")" -> "]")
    if (trimmed.isEmpty) " "
    else trimmed
  }
}
/**
 * Utility class grouping the total description + map of each fragment to its description
 */
case class DescriptionAndExamples[T](description: Description, descriptions: Map[T, Description])
