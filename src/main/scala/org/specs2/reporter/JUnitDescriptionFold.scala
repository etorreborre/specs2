package org.specs2
package reporter
import io._
import specification._
import _root_.org.junit.runner._
import scalaz._
import Scalaz._

/**
 * The JUnit description Fold transforms a list of fragments
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
class JUnitDescriptionFold(specificationClass: Class[_]) extends Fold {
	
  /** 
   * A TreeFold which maps nodes to Descriptions
   * 
   * It is important to note that only Text, Examples and Steps 
   * are mapped to Description with the special case of Steps
   * to be translated to "silent" Descriptions which will be executed but 
   * not outputed as JUnit Description nodes when run 
   */
  object descriptionTree extends TreeFold[Description] {
	def root = createSuiteDescription(specificationClass.getSimpleName)
	def map: Function[Fragment, Option[Description]] = {
	  case Text(t) => Some(createSuiteDescription(testName(t)))
      case Example(description, body) =>  Some(createDescription(testName(description)))
      case Step(action) => Some(createDescription("step"))
      case other => None
	}
  }
  case class AccumulatedDescription(val description: descriptionTree.T, val executions: Map[Description, Fragment])
  object DescriptionAndExamples {
	def unapply(acc: AccumulatedDescription): Option[(Description, Map[Description, Fragment])] =
	  acc match {
		case AccumulatedDescription(description, executions) => {
		  val descriptionTree.Tree(tree) = description
		  Some((asOneDescription(tree), executions)) 
		}
	  }
  }
  override type T = AccumulatedDescription
  val initial = new AccumulatedDescription(descriptionTree.initial, Map.empty[Description, Fragment])
  
  val fold = (descExamples: T, f: Fragment) => {
	val AccumulatedDescription(treeLoc, examples) = descExamples
	val newTreeLoc = descriptionTree.fold(treeLoc, f)
	val newExamples = f match {
      case Step(action) => examples + (createDescription("step") -> f)
      case Text(t) => examples + (createSuiteDescription(testName(t)) -> f)
      case Example(description, body) =>  examples + (createDescription(testName(description)) -> f)
      case _ => examples
	}
	new AccumulatedDescription(newTreeLoc, newExamples)
  }
  
  def toDescription(fragments: Fragment*): Description = asOneDescription(descriptionTree.fold(fragments:_*).tree)
  def asOneDescription(descriptionTree: Tree[Description]): Description = {
    val addChildren = (d: Description, children: Stream[Description]) => { children.foreach(d.addChild(_)); d }
    TreeFold.bottomUp(descriptionTree, addChildren).rootLabel
  }
  
  private def testName(s: String)= {
	(if (s contains "\n") (s.trim.split("\n")(0) + "...") else s.trim).replaceAll("\r", "")
  }
  private def sanitize(s: String) = s.replace("(", "[").replace(")", "]")
  private def createDescription(s: String) = Description.createTestDescription(specificationClass, sanitize(s))
  private def createSuiteDescription(s: String) = Description.createSuiteDescription(sanitize(s))
}

