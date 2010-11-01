package org.specs2
package reporter

import _root_.org.junit.runner._
import scalaz._
import Scalaz._
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
class JUnitDescriptionFold(specificationClass: Class[_]) extends Fold {
	
  /** 
   * A TreeFold which maps nodes to Descriptions
   * 
   * It is important to note that only Text, Examples and Steps 
   * are mapped to Description with the special case of Steps
   * to be translated to "silent" Descriptions which will be executed but 
   * not outputed as JUnit Description nodes when run 
   */
  lazy val descriptionTree = new TreeFold[Description] {
	  def root = createSuiteDescription(specificationClass.getSimpleName)
	  def optFold: Function2[T, Fragment, Option[Description]] = {
	    case (a, Text(t)) => Some(createSuiteDescription(testName(t)))
      case (a, Example(description, body)) =>  Some(createDescription(testName(description), a.rootTree.flatten.size))
      case (a, Step(action)) => Some(createDescription("step", a.rootTree.flatten.size))
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
      case Step(action) => examples + (newTreeLoc.label -> f)
      case Text(t) => examples + (newTreeLoc.label -> f)
      case Example(description, body) => examples + (newTreeLoc.label -> f)
      case _ => examples
    }
	  
	  new AccumulatedDescription(newTreeLoc, newExamples)
  }
  
  def toDescription(fragments: Fragment*): Description = asOneDescription(descriptionTree.fold(fragments:_*).rootTree)
  def asOneDescription(descriptionTree: Tree[Description]): Description = {
    val addChildren = (d: Description, children: Stream[Description]) => { children.foreach(d.addChild(_)); d }
    TreeFold.bottomUp(descriptionTree, addChildren).rootLabel
  }
  
  private def testName(s: String)= {
	  s.trim.replaceAll("\r", "").replaceAll("\n", "")
  }
  private def sanitize(s: String) = s.trim.replace("(", "[").replace(")", "]")
  private def createDescription(s: String, e: Any) = Description.createSuiteDescription(sanitize(s)+"("+e.toString+")")
  private def createSuiteDescription(s: String) = Description.createSuiteDescription(sanitize(s))
}

