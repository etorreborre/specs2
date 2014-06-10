package org.specs2
package reporter

import org.specs2.specification.create.FragmentsFactory
import org.specs2.specification.dsl.FragmentsDsl
import specification.core._
import matcher.{MustMatchers, StandardMatchResults}
import data.Trees._
import org.junit.runner.Description
import execute.{Result, Success, StandardResults}
import ShowDescription._

class JUnitDescriptionSpec extends Specification with JUnitDescriptionSpecTest { def is = s2"""
                                                                                         
 A list of Fragments can be 'folded' into a tree of JUnit descriptions so that there is
 a root Description object (the top 'suite') and children objects representing either
 nested suites or Tests.
                                                                                        
 The list must be properly folded to a Descriptions tree                                                             
                                                                                                                       
   An example is folded into a root description for the spec class and a description of the example                 $a1
   Two examples are folded as 2 children descriptions                                                               $a2
   A text and two subordinates examples are folded as a node and 2 children descriptions                            $a3
   2 texts and two subordinates examples each are folded as 2 nodes and with their own children descriptions        $a4
   2 groups of examples separated by a paragraph are folded as 2 nodes and with their own children descriptions     $a5
   2 groups of examples and a separate one are folded as 2 suites and one test case                                 $a6
   An example then a text grouping 2 examples are folded as 1 suite, with one test and 1 suite with 2 test cases    $a7
   An example description must not have newlines if executed from an IDE                                            $a8
   Empty descriptions must be removed from the tree                                                                 $a9

 The Descriptions objects must have proper details
   For as single Example                                                                                            $b1

   For a Text followed by examples
     for the text                                                                                                   $b2
     for the first example                                                                                          $b3
                                                                                                                    """

  import ReporterExamples._

  def a1 = descriptionIs(ex1)(
    "JUnitDescriptionSpec",
    "|",
    "`- ex1(org.specs2.reporter.JUnitDescriptionSpec)\n")

  def a2 = descriptionIs(ex1 ^ ex2)(
    "JUnitDescriptionSpec",
    "|",
    "+- ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "|",
    "`- ex2(org.specs2.reporter.JUnitDescriptionSpec)\n")

  def a3 = descriptionIs(level1)(
    "JUnitDescriptionSpec",
    "|",
    "`- level1",
    "   |",
    "   +- level1::ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "   |",
    "   `- level1::ex2(org.specs2.reporter.JUnitDescriptionSpec)\n")

  def a4 = descriptionIs(level1Level2)(
    "JUnitDescriptionSpec",
    "|",
    "+- level1",
    "|  |",
    "|  +- level1::ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "|  |",
    "|  `- level1::ex2(org.specs2.reporter.JUnitDescriptionSpec)",
    "|",
    "`- level2",
    "   |",
    "   +- level2::ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "   |",
    "   `- level2::ex2(org.specs2.reporter.JUnitDescriptionSpec)\n")

  def a5 = descriptionIs(level1 ^ break ^ backtab ^ level2)(
    "JUnitDescriptionSpec",
    "|",
    "+- level1",
    "|  |",
    "|  +- level1::ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "|  |",
    "|  `- level1::ex2(org.specs2.reporter.JUnitDescriptionSpec)",
    "|",
    "`- level2",
    "   |",
    "   +- level2::ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "   |",
    "   `- level2::ex2(org.specs2.reporter.JUnitDescriptionSpec)\n")

  def a6 = descriptionIs(level1 ^ ex3)(
    "JUnitDescriptionSpec",
    "|",
    "+- level1",
    "|  |",
    "|  +- level1::ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "|  |",
    "|  `- level1::ex2(org.specs2.reporter.JUnitDescriptionSpec)",
    "|",
    "`- ex3(org.specs2.reporter.JUnitDescriptionSpec)\n")

  def a7 = descriptionIs(ex1 ^ level1)(
    "JUnitDescriptionSpec",
    "|",
    "+- ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "|",
    "`- level1",
    "   |",
    "   +- level1::ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "   |",
    "   `- level1::ex2(org.specs2.reporter.JUnitDescriptionSpec)\n")

  def a8 = descriptionIs(helloWorld, fromIDE = true)(
    "JUnitDescriptionSpec",
    "|",
    "`- hello world(2)\n")

  def a9 = descriptionIs(level1 ^ empty1)(
    "JUnitDescriptionSpec",
    "|",
    "`- level1",
    "   |",
    "   +- level1::ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    "   |",
    "   `- level1::ex2(org.specs2.reporter.JUnitDescriptionSpec)\n")

  def b1 = details (
    description   = toDescription(ex1).getChildren.get(0),
    className     = "org.specs2.reporter.JUnitDescriptionSpec",
    methodName    = "ex1",
    testClass     = classOf[JUnitDescriptionSpec],
    displayName   = "ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    isTest        = true
  )

  def b2 = details (
    description   = toDescription(level1).getChildren.get(0),
    className     = "level1",
    methodName    = null,
    testClass     = null,
    displayName   = "level1",
    isTest        = false
  )

  def b3 = details (
    description   = toDescription(level1).getChildren.get(0).getChildren.get(0),
    className     = "org.specs2.reporter.JUnitDescriptionSpec",
    methodName    = "level1::ex1",
    testClass     = classOf[JUnitDescriptionSpec],
    displayName   = "level1::ex1(org.specs2.reporter.JUnitDescriptionSpec)",
    isTest        = true
  )

  def descriptionIs(f: Fragment, fromIDE: Boolean)(tree: String*): Result =
    descriptionIs(Fragments(f), fromIDE)(tree:_*)

  def descriptionIs(f: Fragment)(tree: String*): Result =
    descriptionIs(Fragments(f))(tree:_*)

  def descriptionIs(fs: Fragments, fromIDE: Boolean = false)(tree: String*): Result =
    showDescriptionTree(titled(fs), fromIDE) must_== tree.toList.mkString("\n")

  def showDescriptionTree(spec: SpecStructure, fromIDE: Boolean = false): String = {
    // set the header to the main specification class
    val newHeader = spec.header.copy(specClass = classOf[JUnitDescriptionSpec])
    descriptions(fromIDE).createDescription(spec.copy(header = newHeader)).drawTree
  }

  def toDescription(f: Fragment): Description   = toDescription(Fragments(f))
  def toDescription(fs: Fragments): Description = {
    val spec = titled(fs)

    // set the header to the main specification class
    val newHeader = spec.header.copy(specClass = classOf[JUnitDescriptionSpec])
    descriptions().createDescription(spec.copy(header = newHeader))
  }


  def descriptions(fromIDE: Boolean = false) = new JUnitDescriptions {
    override lazy val isExecutedFromAnIDE = fromIDE
  }
}

trait ReporterExamples extends MustMatchers with StandardResults with StandardMatchResults with FragmentsFactory with FragmentsDsl {
  private val factory = fragmentFactory; import factory._

  lazy val text = factory.text("text")

  def exOk(i: Int) = "ex"+i ! {println("ex"+i);success}

  lazy val ex1 = "ex1" ! success
  lazy val ex2 = "ex2" ! success
  lazy val ex3 = "ex3" ! success
  lazy val empty1 = factory.text("     ")
  lazy val ex1Failure = "ex1" ! failure
  lazy val ex1BeEqualToFailure = "ex1" ! { 1 must_== 2 }
  lazy val ex1Error = "ex1" ! anError
  lazy val ex1Skipped  = "ex1" ! skipped
  lazy val ex1Pending  = "ex1" ! pending
  lazy val exampleWithExpectations = "ex1" ! Success("ok", 2)

  lazy val level1 = start ^ "level1" ^ break ^ ex1 ^ ex2 ^ end
  lazy val level2 = start ^ "level2" ^ break ^ ex1 ^ ex2 ^ end
  lazy val level2WithFailure = start ^ "level2" ^ break ^ ex1Failure ^ ex2 ^ end
  lazy val level1Level2 = level1 ^ level2
  lazy val level1ParLevel2 = level1 ^ break ^ backtab ^ level2

  lazy val helloWorld = "hello \nworld" ! success

  def titled(fs: Fragments) = "JUnitDescriptionSpec".title ^ fs

}

object ReporterExamples extends ReporterExamples

trait JUnitDescriptionSpecTest extends Specification {

  def details(description: Description, className: String, methodName: String, testClass: Class[_], displayName: String, isTest: Boolean) = {
      p^
      "the className must be filled"    ! desc(description).e1(className)   ^br^
      "the methodName must be correct"  ! desc(description).e2(methodName)  ^br^
      "the testClass must be correct"   ! desc(description).e3(testClass)   ^br^
      "the displayName must be filled"  ! desc(description).e4(displayName) ^br^
      "the isTest flag must be correct" ! desc(description).e5(isTest)      ^br
  }

  case class desc(description: Description) {
    def e1(name: String)     = description.getClassName must_== name
    def e2(name: String)     = description.getMethodName must_== name
    def e3(klass: Class[_])  = (description.getTestClass:Any) must_== klass
    def e4(name: String)     = description.getDisplayName must_== name
    def e5(isTest: Boolean)  = description.isTest must_== isTest
  }
}
