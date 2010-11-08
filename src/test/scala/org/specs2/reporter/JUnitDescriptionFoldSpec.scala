package org.specs2
package reporter
import specification._
import org.junit.runner._
import scalaz._
import Scalaz._
import ShowDescription._
import scala.collection.JavaConversions._

class JUnitDescriptionFoldSpec extends SpecificationWithJUnit with FragmentsSamples {  def is =
  
                                                                                          """
  A list of Fragments can be 'folded' into a tree of JUnit descriptions so that there is  
  a root Description object (the top 'suite') and children objects representing either 
  nested suites or Tests
                                                                                          """                                                                                                               ^
"  An example is folded into a root description for the spec class and a description of"  +  
"  the example"                                                                           ! e1^
                                                                                          p^
"  Two examples are folded as 2 children descriptions"                                    ! e2^
                                                                                          p^
"  A text and two subordinates examples are folded as a node and 2 children descriptions" ! e3^
                                                                                          p^
"  2 texts and two subordinates examples each are folded as 2 nodes and with their own "  +
"  children descriptions"                                                                 ! e4^
                                                                                          p^
"  2 groups of examples separated by a paragraph are folded as 2 nodes and with their "   +
"  own children descriptions"                                                             ! e5^
                                                                                          p^
"  2 grouped examples and a separate one are folded as 2 suites and one test case"        ! e6^
"  if 2 fragments have the same name, they must have a different description"             ! e7^
                                                                                          p^
                                                                                          end

  def e1 = descriptionIs(ex1)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "`- ex1(1)\n")
  		   
  def e2 = descriptionIs(ex1 ^ ex2)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "+- ex1(1)",
  		   "|",
  		   "`- ex2(2)\n")
  		   
  def e3 = descriptionIs(level1)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "`- level1",
  		   "   |",
  		   "   +- ex1(2)",
  		   "   |",
  		   "   `- ex2(3)\n")

  def e4 = descriptionIs(level1Level2)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(2)",
  		   "|  |",
  		   "|  `- ex2(3)",
  		   "|",
  		   "`- level2",
  		   "   |",
  		   "   +- ex1(5)",
  		   "   |",
  		   "   `- ex2(6)\n")

  def e5 = descriptionIs(level1ParLevel2)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(2)",
  		   "|  |",
  		   "|  `- ex2(3)",
  		   "|",
  		   "`- level2",
  		   "   |",
  		   "   +- ex1(5)",
  		   "   |",
  		   "   `- ex2(6)\n")
  		   
  def e6 = descriptionIs(level1 ^ end ^ ex3)(
  		   "JUnitDescriptionFoldSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(2)",
  		   "|  |",
  		   "|  `- ex2(3)",
  		   "|",
  		   "`- ex3(4)\n")

  def e7 = descriptionIs(ex1 ^ ex1)(
         "JUnitDescriptionFoldSpec",
         "|",
         "+- ex1(1)",
         "|",
         "`- ex1(2)\n")

  def descriptionIs(f: Fragments)(tree: String*) = 
	  showDescriptionTree(f.fragments) must_== tree.toList.mkString("\n")
  
  def showDescriptionTree(fragments: Seq[Fragment]): String = 
    toDescription(fragments:_*).drawTree
  
  def toDescription(fragments: Fragment*): Description = {
    val fold = new JUnitDescriptionFold(getClass)
    fold.asOneDescription(fold.descriptionTree.foldAll(fragments)(main.Arguments()).rootTree)
  }
    

}
