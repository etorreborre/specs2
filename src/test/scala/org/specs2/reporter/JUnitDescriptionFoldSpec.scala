package org.specs2
package reporter
import specification._
import org.junit.runner._
import scalaz._
import Scalaz._
import ShowDescription._
import scala.collection.JavaConversions._

class JUnitDescriptionFoldSpec extends SpecificationWithJUnit with FragmentsSamples {
  val content = 
"""
  A list of Fragments can be 'folded' into a tree of JUnit descriptions so that there is  
  a root Description object (the top 'suite') and children objects representing either 
  nested suites or Tests
"""^
" An example is folded into a root description for the spec class and a description of the example" ! e1^
" Two examples are folded as 2 children descriptions" ! e2^
" A text and two subordinates examples are folded as a node and 2 children descriptions" ! e3^
" 2 texts and two subordinates examples each are folded as 2 nodes and with their own children descriptions" ! e4^
" 2 groups of examples separated by a paragraph are folded as 2 nodes and with their own children descriptions" ! e5^
" 2 grouped examples and a separate one" ! e6^
end

  def e1 = descriptionIs(ex1)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "`- ex1(org.specs2.reporter.JUnitDescriptionFoldSpec)\n")
  		   
  def e2 = descriptionIs(ex1 ^ ex2)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "+- ex1(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "|",
  		   "`- ex2(org.specs2.reporter.JUnitDescriptionFoldSpec)\n")
  		   
  def e3 = descriptionIs(level1)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "`- level1",
  		   "   |",
  		   "   +- ex1(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "   |",
  		   "   `- ex2(org.specs2.reporter.JUnitDescriptionFoldSpec)\n")

  def e4 = descriptionIs(level1Level2)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "|  |",
  		   "|  `- ex2(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "|",
  		   "`- level2",
  		   "   |",
  		   "   +- ex1(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "   |",
  		   "   `- ex2(org.specs2.reporter.JUnitDescriptionFoldSpec)\n")

  def e5 = descriptionIs(level1ParLevel2)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "|  |",
  		   "|  `- ex2(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "|",
  		   "`- level2",
  		   "   |",
  		   "   +- ex1(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "   |",
  		   "   `- ex2(org.specs2.reporter.JUnitDescriptionFoldSpec)\n")
  		   
  def e6 = descriptionIs(level1 ^ end ^ ex3)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "|  |",
  		   "|  `- ex2(org.specs2.reporter.JUnitDescriptionFoldSpec)",
  		   "|",
  		   "`- ex3(org.specs2.reporter.JUnitDescriptionFoldSpec)\n")

  def descriptionIs(f: Fragments)(tree: String*) = 
	showDescriptionTree(f.fragments) must_== tree.toList.mkString("\n")
  
  def showDescriptionTree(fragments: List[Fragment]): String = {
	new JUnitDescriptionFold(getClass).toDescription(fragments:_*).drawTree
  }
}
