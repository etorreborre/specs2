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
 * * a Map of Fragments to execute, or executed fragments, indexed by Description: Description -> Fragment
 * 
 * The Description object creation works by using the Levels reducer to build a Tree[Description].
 * That Tree is then folded bottom-up to create the necessary associations between the 
 * Description objects. 
 * 
 */
private[specs2]
abstract class JUnitDescriptions[F](specificationClass: Class[_])(implicit reducer: Reducer[F, Levels[F]]) extends JUnitDescriptionMaker[F] {
  import Levels._

  /**
   * @return fragment that must be used as the root description if the specification is empty
   */
  def initialFragment(s: Class[_]): F

  implicit lazy val initial = (specificationDescription, initialFragment(specificationClass))

  def foldAll(fs: Seq[F])(implicit args: Arguments) = {
    val leveledFragments = Levels.foldAll(fs)

    if (leveledFragments.isEmpty)
      DescriptionAndExamples(specificationDescription, Seq(initial).toStream)
    else {
      val descriptionTree = leveledFragments.toTree[DescribedFragment](mapper(specificationClass))
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

  lazy val specificationDescription = createDescription(specificationClass, specificationClass.getName)
}

private[specs2]
trait JUnitDescriptionMaker[F] extends ExecutionOrigin {
  type DescribedFragment = (Description, F)
  /**
   * This function is used to map each node in a Tree[Fragment] to a pair of 
   * (Description, Fragment)
   * 
   * The Int argument is the numeric label of the current TreeNode being mapped.
   * It is used to create a unique description of the example to executed which is required
   * by JUnit
   */
  def mapper(klass: Class[_]): (F, Seq[DescribedFragment], Int) => Option[DescribedFragment]
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
  private val addChildren = (desc: (Description, F), children: Stream[DescribedFragment]) => {
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
      if (testName.isEmpty) (if (suiteName.isEmpty) testClass.getName else suiteName)
      else sanitize(testName)+"("+origin+")"
    
    Description.createSuiteDescription(desc)
  }

  import text.Trim._

  /** @return a seq containing the path of an example without the root name */
  def parentPath(parentNodes: Seq[DescribedFragment]) = parentNodes.drop(1).map(_._1.getDisplayName)

  /** @return a test name with no newlines */
  def testName(s: String, parentNodes: Seq[String] = Seq()): String = {
    (if (parentNodes.isEmpty || isExecutedFromAnIDE) "" else parentNodes.mkString("", "::", "::")) +
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
 * Utility class grouping the total description + fragments to execute for each Description
 */
case class DescriptionAndExamples[T](val description: Description, executions: Stream[T])
