package org.specs2
package reporter
import specification._
import org.junit.runner._
import scalaz._
import Scalaz._
import ShowDescription._
import scala.collection.JavaConversions._

class JUnitDescriptionsSpec extends Specification with FragmentsSamples {  def is =
                                                                                                                        """
  A list of Fragments can be 'folded' into a tree of JUnit descriptions so that there is
  a root Description object (the top 'suite') and children objects representing either
  nested suites or Tests.
                                                                                                                        """^
                                                                                                                        p^
  "An example is folded into a root description for the spec class and a description of the example"                    ! e1^
                                                                                                                        p^
  "Two examples are folded as 2 children descriptions"                                                                  ! e2^
                                                                                                                        p^
  "A text and two subordinates examples are folded as a node and 2 children descriptions"                               ! e3^
                                                                                                                        p^
  "2 texts and two subordinates examples each are folded as 2 nodes and with their own children descriptions"           ! e4^
                                                                                                                        p^
  "2 groups of examples separated by a paragraph are folded as 2 nodes and with their "                                 ^bt^
  "own children descriptions"                                                                                           ! e5^
                                                                                                                        p^
  "2 grouped examples and a separate one are folded as 2 suites and one test case"                                      ! e6^
                                                                                                                        p^
  "An example followed by a text grouping 2 examples are folded as 1 suite, with one test"                              +
  "and 1 suite with 2 test cases"                                                                                       ! e7^
                                                                                                                        p^
  "If 2 fragments have the same name, they must have a different description"                                           ! e8^
                                                                                                                        end

  def e1 = descriptionIs(ex1)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "`- ex1(1)\n")
  		   
  def e2 = descriptionIs(ex1 ^ ex2)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "+- ex1(1)",
  		   "|",
  		   "`- ex2(2)\n")
  		   
  def e3 = descriptionIs(level1)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "`- level1",
  		   "   |",
  		   "   +- ex1(2)",
  		   "   |",
  		   "   `- ex2(3)\n")

  def e4 = descriptionIs(level1Level2)(
  		   "JUnitDescriptionsSpec",
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

  def e5 = descriptionIs("level1" ^ ex1 ^ ex2 ^ p ^ "level2" ^ ex1 ^ ex2)(
  		   "JUnitDescriptionsSpec",
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
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(2)",
  		   "|  |",
  		   "|  `- ex2(3)",
  		   "|",
  		   "`- ex3(4)\n")

  def e7 = descriptionIs(ex1 ^ "t1" ^ ex1 ^ ex2)(
         "JUnitDescriptionsSpec",
         "|",
         "+- ex1(1)",
         "|",
         "`- t1",
         "   |",
         "   +- ex1(3)",
         "   |",
         "   `- ex2(4)\n")

  def e8 = descriptionIs(ex1 ^ ex1)(
         "JUnitDescriptionsSpec",
         "|",
         "+- ex1(1)",
         "|",
         "`- ex1(2)\n")

  def descriptionIs(f: Fragments)(tree: String*) = 
	  showDescriptionTree("JUnitDescriptionsSpec".title ^ f) must_== tree.toList.mkString("\n")
  
  def showDescriptionTree(fragments: Fragments): String = 
    toDescription(fragments.fragments:_*).drawTree
  
  def toDescription(fragments: Fragment*): Description = {
    import Levels._
    val descriptionTree = foldAll(fragments).toTree(JUnitDescriptions.mapper("JUnitDescriptionSpec"))
    JUnitDescriptions.asOneDescription(descriptionTree)
  }
    

}
