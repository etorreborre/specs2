package org.specs2
package reporter

import _root_.org.junit.runner._
import scalaz._
import Scalaz._
import text.Trim._
import main.Arguments
import specification._

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
class JUnitDescriptionFold(specificationClass: Class[_]) extends FragmentFold {
	
  /** 
   * A TreeFold which maps nodes to Descriptions
   * 
   * It is important to note that only Text, Examples and Steps 
   * are mapped to Description with the special case of Steps
   * to be translated to "silent" Descriptions which will be executed but 
   * not output as JUnit Description nodes when run 
   */
  lazy val descriptionTree = new TreeFold[Description] {
    /**
     * root of the tree: the class name
     */
	  def root = createSuiteDescription(specificationClass.getSimpleName)
	  /**
	   * map only Text, Examples and Steps to junit Descriptions
	   */
	  def optFold: Function2[T, Fragment, Option[Description]] = {
	    case (a, Text(t)) => Some(createSuiteDescription(testName(t)))
      case (a, Example(description, body)) =>  Some(createDescription(testName(description), a.rootTree.flatten.size))
      case (a, Step(action)) => Some(createDescription("step", a.rootTree.flatten.size))
      case other => None
	  }
  }

  /** 
   * Enriched accumulator type: descriptions + fragment to execute for each Description 
   */
  override type T = AccumulatedDescription
  case class AccumulatedDescription(val description: descriptionTree.T, val executions: Map[Description, Fragment])
  val initial = new AccumulatedDescription(descriptionTree.initial, Map.empty[Description, Fragment])
  
  /**
   * @return a folding function which creates a tree of description objects and a map of 
   *         Fragments to execute
   */
  def fold(implicit arguments: Arguments) = (descExamples: T, f: Fragment) => {
	  val AccumulatedDescription(treeLoc, examples) = descExamples
	  val newTreeLoc = descriptionTree.fold(arguments)(treeLoc, f)
	  val newExamples = f match {
      case Step(action) => examples + (newTreeLoc.label -> f)
      case Text(t) => examples + (newTreeLoc.label -> f)
      case Example(description, body) => examples + (newTreeLoc.label -> f)
      case _ => examples
    }
	  
	  new AccumulatedDescription(newTreeLoc, newExamples)
  }

  /**
   * @return a Description with parent-child relationships to other Description objects
   *         from a Tree[Description]
   */
  def asOneDescription(descriptionTree: Tree[Description]): Description = {
    val addChildren = (d: Description, children: Stream[Description]) => { children.foreach(d.addChild(_)); d }
    TreeFold.bottomUp(descriptionTree, addChildren).rootLabel
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

  /**
   * Extractor object for a tree of descriptions and a map of executions
   */
  object DescriptionAndExamples {
    def unapply(acc: AccumulatedDescription): Option[(Description, Map[Description, Fragment])] =
      acc match {
        case AccumulatedDescription(description, executions) => {
          val descriptionTree.Tree(tree) = description
          Some((asOneDescription(tree), executions)) 
        }
      }
  }
}

