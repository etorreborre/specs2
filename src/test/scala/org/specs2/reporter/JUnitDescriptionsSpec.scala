package org.specs2
package reporter
import specification._
import org.junit.runner._
import ShowDescription._

class JUnitDescriptionsSpec extends Specification with FragmentsSamples with  Tags {  def is =
                                                                                                                        """
  A list of Fragments can be 'folded' into a tree of JUnit descriptions so that there is
  a root Description object (the top 'suite') and children objects representing either
  nested suites or Tests.
                                                                                                                        """^
  "The list must be properly folded to a Descriptions tree"                                                             ^
                                                                                                                        p^
    "An example is folded into a root description for the spec class and a description of the example"                  ! e1^
    "Two examples are folded as 2 children descriptions"                                                                ! e2^
    "A text and two subordinates examples are folded as a node and 2 children descriptions"                             ! e3^
    "2 texts and two subordinates examples each are folded as 2 nodes and with their own children descriptions"         ! e4^
    "2 groups of examples separated by a paragraph are folded as 2 nodes and with their own children descriptions"      ! e5^
    "2 groups of examples and a separate one are folded as 2 suites and one test case"                                  ! e6^
    "An example then a text grouping 2 examples are folded as 1 suite, with one test and 1 suite with 2 test cases"     ! e7^
                                                                                                                        p^
  "The Descriptions objects must have proper details"                                                                   ^
    "For an Example"                                                                                                    ^
      "the className must be filled"                                                                                    ! desc.e1^
      "the methodName must be filled"                                                                                   ! desc.e2^
      "the testClassName must be filled"                                                                                ! desc.e3^
      "the displayName must be filled"                                                                                  ! desc.e4^
      "the isTest flag must be true"                                                                                    ! desc.e5^
                                                                                                                        p^
    "For a Text followed by examples"                                                                                   ^
      "the className must be filled"                                                                                    ! desc.e6^
      "the methodName must be filled"                                                                                   ! desc.e7^
      "the testClassName must be filled"                                                                                ! desc.e8^
      "the displayName must be filled"                                                                                  ! desc.e9^
      "the isSuite flag must be true"                                                                                   ! desc.e10^
      "For the first example"                                                                                           ^
        "the className must be filled"                                                                                  ! desc.e11^
        "the methodName must be filled"                                                                                 ! desc.e12^
        "the testClassName must be filled"                                                                              ! desc.e13^
        "the displayName must be filled"                                                                                ! desc.e14^
        "the isTest must be true"                                                                                       ! desc.e15^
                                                                                                                        end

  def e1 = descriptionIs(ex1)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "`- ex1(org.specs2.reporter.JUnitDescriptionsSpec)\n")
  		   
  def e2 = descriptionIs(ex1 ^ ex2)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "+- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|",
  		   "`- ex2(org.specs2.reporter.JUnitDescriptionsSpec)\n")
  		   
  def e3 = descriptionIs(level1)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "`- level1",
  		   "   |",
  		   "   +- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "   |",
  		   "   `- ex2(org.specs2.reporter.JUnitDescriptionsSpec)\n")

  def e4 = descriptionIs(level1Level2)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|  |",
  		   "|  `- ex2(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|",
  		   "`- level2",
  		   "   |",
  		   "   +- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "   |",
  		   "   `- ex2(org.specs2.reporter.JUnitDescriptionsSpec)\n")

  def e5 = descriptionIs("level1" ^ ex1 ^ ex2 ^ p ^ "level2" ^ ex1 ^ ex2)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|  |",
  		   "|  `- ex2(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|",
  		   "`- level2",
  		   "   |",
  		   "   +- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "   |",
  		   "   `- ex2(org.specs2.reporter.JUnitDescriptionsSpec)\n")
  		   
  def e6 = descriptionIs(level1 ^ end ^ ex3)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|  |",
  		   "|  `- ex2(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|",
  		   "`- ex3(org.specs2.reporter.JUnitDescriptionsSpec)\n")

  def e7 = descriptionIs(ex1 ^ "t1" ^ ex1 ^ ex2)(
         "JUnitDescriptionsSpec",
         "|",
         "+- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
         "|",
         "`- t1",
         "   |",
         "   +- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
         "   |",
         "   `- ex2(org.specs2.reporter.JUnitDescriptionsSpec)\n")

  def e8 = descriptionIs(ex1 ^ ex1)(
         "JUnitDescriptionsSpec",
         "|",
         "+- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
         "|",
         "`- ex1(org.specs2.reporter.JUnitDescriptionsSpec)\n")

  def descriptionIs(f: Fragments)(tree: String*) = 
	  showDescriptionTree("JUnitDescriptionsSpec".title ^ f) must_== tree.toList.mkString("\n")
  
  def showDescriptionTree(fragments: Fragments): String = 
    toDescription(fragments).drawTree
  
  def toDescription(fragments: Fragments): Description = toDescription(fragments.fragments:_*)
  def toDescription(fragments: Fragment*): Description = {
    import Levels._
    val descriptionTree = foldAll(fragments).toTree(descriptionsMaker.mapper(classOf[JUnitDescriptionsSpec]))
    descriptionsMaker.asOneDescription(descriptionTree)
  }
  val descriptionsMaker = new JUnitDescriptionMaker {
    override lazy val isExecutedFromAnIDE = false
  }
  object desc {

    val test = toDescription("e1"! success)
    def e1  = test.getClassName must_== "org.specs2.reporter.JUnitDescriptionsSpec"
    def e2  = test.getMethodName must_== "e1"
    def e3  = (test.getTestClass:Any) must_== classOf[JUnitDescriptionsSpec]
    def e4  = test.getDisplayName must_== "e1(org.specs2.reporter.JUnitDescriptionsSpec)"
    def e5  = test.isTest must beTrue

    val suite = toDescription("t1" ^ "e1"! success).getChildren.get(0)
    def e6  = suite.getClassName must_== "t1"
    def e7  = suite.getMethodName must beNull
    def e8  = (suite.getTestClass:Any) must beNull
    def e9  = suite.getDisplayName must_== "t1"
    def e10 = suite.isTest must beFalse

    val testChild = suite.getChildren.get(0)
    def e11 = testChild.getClassName must_== "org.specs2.reporter.JUnitDescriptionsSpec"
    def e12 = testChild.getMethodName must_== "e1"
    def e13 = (testChild.getTestClass:Any) must_== classOf[JUnitDescriptionsSpec]
    def e14 = testChild.getDisplayName must_== "e1(org.specs2.reporter.JUnitDescriptionsSpec)"
    def e15 = testChild.isTest must beTrue
  }
}
