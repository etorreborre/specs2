package org.specs2
package reporter
import specification._
import org.junit.runner._
import ShowDescription._
import runner.JUnitDescriptionsFragments

class JUnitDescriptionsSpec extends Specification with FragmentsSamples {  def is =
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
    "An example description must not have newlines if executed from an IDE"                                             ! e8^
                                                                                                                        p^
  "The Descriptions objects must have proper details"                                                                   ^
    "For as single Example"                                                                                             ^
      details (
        description   = toDescription("e1"! success),
        className     = "org.specs2.reporter.JUnitDescriptionsSpec",
        methodName    = "e1",
        testClass     = classOf[JUnitDescriptionsSpec],
        displayName   = "e1(org.specs2.reporter.JUnitDescriptionsSpec)",
        isTest        = true
      )                                                                                                                 ^p^
    "For a Text followed by examples"                                                                                   ^
      "For the text"                                                                                                    ^
        details (
          description   = toDescription("t1" ^ "e1"! success).getChildren.get(0),
          className     = "t1",
          methodName    = null,
          testClass     = null,
          displayName   = "t1",
          isTest        = false
        )                                                                                                               ^p^
      "For the first example"                                                                                           ^
        details (
          description   = toDescription("t1" ^ "e1"! success).getChildren.get(0).getChildren.get(0),
          className     = "org.specs2.reporter.JUnitDescriptionsSpec",
          methodName    = "t1::e1",
          testClass     = classOf[JUnitDescriptionsSpec],
          displayName   = "t1::e1(org.specs2.reporter.JUnitDescriptionsSpec)",
          isTest        = true
        )                                                                                                               ^p^
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
  		   "   +- level1::ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "   |",
  		   "   `- level1::ex2(org.specs2.reporter.JUnitDescriptionsSpec)\n")

  def e4 = descriptionIs(level1Level2)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- level1::ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|  |",
  		   "|  `- level1::ex2(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|",
  		   "`- level2",
  		   "   |",
  		   "   +- level2::ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "   |",
  		   "   `- level2::ex2(org.specs2.reporter.JUnitDescriptionsSpec)\n")

  def e5 = descriptionIs("level1" ^ ex1 ^ ex2 ^ p ^ "level2" ^ ex1 ^ ex2)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- level1::ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|  |",
  		   "|  `- level1::ex2(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|",
  		   "`- level2",
  		   "   |",
  		   "   +- level2::ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "   |",
  		   "   `- level2::ex2(org.specs2.reporter.JUnitDescriptionsSpec)\n")
  		   
  def e6 = descriptionIs(level1 ^ end ^ ex3)(
  		   "JUnitDescriptionsSpec",
  		   "|",
  		   "+- level1",
  		   "|  |",
  		   "|  +- level1::ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|  |",
  		   "|  `- level1::ex2(org.specs2.reporter.JUnitDescriptionsSpec)",
  		   "|",
  		   "`- ex3(org.specs2.reporter.JUnitDescriptionsSpec)\n")

  def e7 = descriptionIs(ex1 ^ "t1" ^ ex1 ^ ex2)(
         "JUnitDescriptionsSpec",
         "|",
         "+- ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
         "|",
         "`- t1",
         "   |",
         "   +- t1::ex1(org.specs2.reporter.JUnitDescriptionsSpec)",
         "   |",
         "   `- t1::ex2(org.specs2.reporter.JUnitDescriptionsSpec)\n")

  def e8 = descriptionIs("hello \nworld" ! success, descriptionsMakerIDE)(
         "JUnitDescriptionsSpec",
         "|",
         "`- hello world(1)\n")

  def descriptionIs(f: Fragments, descMaker: JUnitDescriptionsFragments = descriptionsMaker)(tree: String*) =
    showDescriptionTree("JUnitDescriptionsSpec".title ^ f)(descMaker) must_== tree.toList.mkString("\n")
  
  def showDescriptionTree(fragments: Fragments)(implicit descMaker: JUnitDescriptionsFragments): String = toDescription(fragments)(descMaker).drawTree
  
  def toDescription(fragments: Fragments)(implicit descMaker: JUnitDescriptionsFragments): Description = toDescription(fragments.fragments:_*)(descMaker)
  def toDescription(fragments: Fragment*)(implicit descMaker: JUnitDescriptionsFragments): Description = {
    import Levels._
    val descriptionTree = foldAll(fragments).toTree(descMaker.mapper(classOf[JUnitDescriptionsSpec].getName))
    descMaker.asOneDescription(descriptionTree)
  }

  implicit val descriptionsMaker = new JUnitDescriptionsFragments(getClass.getName) {
    override lazy val isExecutedFromAnIDE = false
  }

  val descriptionsMakerIDE = new JUnitDescriptionsFragments(getClass.getName) {
    override lazy val isExecutedFromAnIDE = true
  }

  def details(description: Description, className: String, methodName: String, testClass: Class[_], displayName: String, isTest: Boolean) = {
    "the className must be filled"    ! desc(description).e1(className)^
    "the methodName must be correct"  ! desc(description).e2(methodName)^
    "the testClass must be correct"   ! desc(description).e3(testClass)^
    "the displayName must be filled"  ! desc(description).e4(displayName)^
    "the isTest flag must be correct" ! desc(description).e5(isTest)
  }
  case class desc(description: Description) {
    def e1(name: String)     = description.getClassName must_== name
    def e2(name: String)     = description.getMethodName must_== name
    def e3(klass: Class[_])  = (description.getTestClass:Any) must_== klass
    def e4(name: String)     = description.getDisplayName must_== name
    def e5(isTest: Boolean)  = description.isTest must_== isTest
  }
}
