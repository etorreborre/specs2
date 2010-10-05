package org.specs2
package reporter
import scalaz._
import Scalaz._
import specification._
import FragmentsShow._
import FragmentsTree._

class TreeFoldSpec extends SpecificationWithJUnit {
  val content = 
  "a specification can be turned to a tree of fragments"^
  "if there is only one text fragment, the tree will have only one leaf" ! {
	toTree("name", List(Text("description"))).drawTree.trim must_== 
    List("SpecStart(name)",
	     "|",
	     "`- Text(description)").mkString("\n")
  }^
  "if there is an example only, the tree will have only one leaf" ! {
	toTree("name", List("description" ! success)).drawTree.trim must_==
    List("SpecStart(name)",
	     "|",
	     "`- Example(description)").mkString("\n")
  }^
  "if there is a text and an example, the tree will have a root and 2 nodes" ! {
	toTree("name", textAndExample.fragments).drawTree.trim must_==
    List("SpecStart(name)",
	     "|",
	     "`- Text(a text)",
	     "   |",
	     "   `- Example(an example)").mkString("\n")
  }^
  "if there is a text and an 2 examples, the 2 examples must be leaves of the text node" ! {
	toTree("name", textAnd2Examples.fragments).drawTree.trim must_==
    List("SpecStart(name)",
	     "|",
	     "`- Text(a text)",
	     "   |",
	     "   +- Example(ex1)",
	     "   |",
	     "   `- Example(ex2)").mkString("\n")
  }^
  "if there are 2 texts and with one example each, each example must be under each text" ! {
	toTree("name", twoTextsAnd1ExampleEach.fragments).drawTree.trim must_==
    List("SpecStart(name)",
	     "|",
	     "+- Text(text1)",
	     "|  |",
	     "|  `- Example(ex1)",
	     "|",
	     "`- Text(text2)",
	     "   |",
	     "   `- Example(ex2)").mkString("\n")
  }^
  "if there are 2 texts and with 2 examples each, all examples must be under each text" ! {
	toTree("name", twoTextsAnd2ExamplesEach.fragments).drawTree.trim must_==
    List("SpecStart(name)",
	     "|",
	     "+- Text(text1)",
	     "|  |",
	     "|  +- Example(ex1.1)",
	     "|  |",
	     "|  `- Example(ex1.2)",
	     "|",
	     "`- Text(text2)",
	     "   |",
	     "   +- Example(ex2.1)",
	     "   |",
	     "   `- Example(ex2.2)").mkString("\n")
  }
  "if there is one text and 2 examples, an end and one text with 1 example, the tree must be well formed" ! {
	toTree("name", oneTextWith2ExamplesAnEndAnd1Example.fragments).drawTree.trim must_==
    List("SpecStart(name)",
	     "|",
	     "+- Text(text1)",
	     "|  |",
	     "|  +- Example(ex1.1)",
	     "|  |",
	     "|  `- Example(ex1.2)",
	     "|",
	     "`- Example(ex2.1)").mkString("\n")
  }
  
  val textAndExample = "a text" ^ "an example" ! success
  val textAnd2Examples = 
	"a text"^ 
	  "ex1" ! success^
	  "ex2" ! success
	  
  val twoTextsAnd1ExampleEach = 	
	"text1"^ "ex1" ! success^
	"text2"^ "ex2" ! success
 
  val twoTextsAnd2ExamplesEach = 
	"text1"^ 
	  "ex1.1" ! success^
	  "ex1.2" ! success^
	"text2"^ 
	  "ex2.1" ! success^
	  "ex2.2" ! success

  val oneTextWith2ExamplesAnEndAnd1Example = 
	"text1"^ 
	  "ex1.1" ! success^
	  "ex1.2" ! success^
	end^
	"ex2.1" ! success
}