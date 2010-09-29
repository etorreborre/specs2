package org.specs2
package reporter
import specification._
import org.junit.runner._
import scalaz._
import Scalaz._
import ShowDescription._
import scala.collection.JavaConversions._

class JUnitDescriptionFoldSpec extends SpecificationWithJUnit with FragmentsSamples {
  val examples = 
"""
  A list of Fragments can be 'folded' into a tree of JUnit descriptions so that there is  
  a root Description object (the top 'suite') and children objects representing either 
  nested suites or Tests
"""^
" An example is folded into a root description and a description of the example" ! e1

  def e1 = showDescriptionTree(ex1) must_== List( 
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "`- ex1(org.specs2.reporter.JUnitDescriptionFoldSpec)\n"
  		   ).mkString("\n")
  
  def showDescriptionTree(fragments: Fragment*): String = {
	def tree(d: Description): Tree[Description] = {
      if (d.getChildren.isEmpty) leaf(d)
      else node(d, d.getChildren.toStream.map(tree(_)))
	}
	tree(new JUnitDescriptionFold(getClass).toDescription(ex1)).drawTree
  }
}
