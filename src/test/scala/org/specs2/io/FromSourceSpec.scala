package org.specs2
package io
import mutable._
import specification.{SpecificationStructure, Example}
import user.specification._

class FromSourceSpec extends Specification with FromSource with Tags {

  "General reading of code".txt
  "An expression can be read from a source file" in {
    examples(spec)(0).desc.toString must contain("1 must_== 1")
  }
  "An expression can be read from a source file even if it spans several lines" in {
    examples(spec)(1).desc.toString must contain("val a") and contain("hello world")
  }
  "Even if the specification name is different from the file name" in {
    examples(new DifferentSpecification)(0).desc.toString must contain("1 must_== 1")
  }
  "If the file is not found, the full path is shown to the user" in {
    other.NotFound.result.toString must be_==("No source file found at src/test/scala/org/specs2/io/other/FromSourceSpec.scala")
  }

  "Special examples cases".txt
  "If there is a function call to an example, the example description should be found" in {
    examples(spec)(2).desc.toString must contain("a call to an example")
  }
  "If the specification doesn't start with a text fragment, the example description should be found" in {
    checkExamples(spec2)
  }
  "If the specification doesn't end with an end fragment, the last example description should be found" in {
    checkExamples(spec3)
  }
  "If there is a function call to a ScalaCheck example, the example description should be found" in {
    examples(scalaCheckSpec)(0).desc.toString must contain("a call to an example")
  }
  "A ScalaCheck expression can be read from a source file even if it spans several lines" in {
    examples(scalaCheckSpec)(1).desc.toString must contain("prop") and contain("a.size")
  }
  "A normal auto example must be read ok in a ScalaCheck spec" in {
    examples(scalaCheckSpec)(2).desc.toString must contain("a normal example")
  }
  "A mutable specification can have auto-examples by annotating results with `.eg`" in {
    val ex = examples(mutableSpec)
    ex must have size (6)
    ex(0).desc.toString must contain("1 === 1")
    ex(1).desc.toString must contain("2 === 2")
    ex(3).desc.toString must contain("an example")
    ex(5).desc.toString must contain("i === 4")
  }
  "An acceptance SpecificationWithJUnit can also have autoexamples" in {
    examples(acceptanceJUnitSpec)(0).desc.toString must contain("1 must_== 1")
  }
  tag("x")
  "A mutable SpecificationWithJUnit can also have autoexamples" in {
    examples(mutableJUnitSpec)(0).desc.toString must contain("1 must_== 1")
  }

  lazy val spec                = new UserFromSourceSpecification
  lazy val spec2               = new SpecificationWithNoStartingText
  lazy val spec3               = new SpecificationWithNoStartingTextAndNoEnd
  lazy val scalaCheckSpec      = new UserFromSourceScalaCheckSpecification
  lazy val mutableSpec         = new MutableSpecificationAutoExamples
  lazy val acceptanceJUnitSpec = new AcceptanceSpecificationWithJUnit
  lazy val mutableJUnitSpec    = new MutableSpecificationWithJUnit

  def checkExamples(spec: SpecificationStructure) = { (e: (Example, Int)) =>
    val index = e._2 + 1
    e._1 must contain(index+" must_== "+index) ^^ ((_:Example).desc.toString)
  }.forall(examples(spec).zipWithIndex)

  def examples(s: SpecificationStructure) = s.is.examples
}

package other {
  object NotFound extends org.specs2.Specification { def is = ""
    def result = success.desc
  }

}
