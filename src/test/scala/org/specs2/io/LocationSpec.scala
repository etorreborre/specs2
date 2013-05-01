package org.specs2
package io

import mutable.Specification
import user.io.{LocationSpecification, LocationUnitSpecification}
import specification.Fragments._
import specification.{SpecificationStructure, Fragment}
import scala.io.Source

class LocationSpec extends Specification {
  "A unit specification must have correct locations for its fragments" >> {
    implicit def spec = new LocationUnitSpecification

    "for the first piece of text, with 'should'" >> {
      textAt(index = 0) === 9
    }
    "for the first example, with 'in'" >> {
      exampleAt(index = 0) === 10
    }
    "for the second example, with 'in'" >> {
      exampleAt(index = 1) === 11
    }
    "for the second piece of text, with '>>'" >> {
      textAt(index = 1) === 14
    }
    "for the 3rd example, with '>>'" >> {
      exampleAt(index = 2) === 15
    }
    "for the 4th example, with '>>'" >> {
      exampleAt(index = 3) === 17
    }
  }

  "An acceptance specification must have correct locations for its fragments" >> {
    implicit def spec = new LocationSpecification

    "for the first piece of text, 'presentation''" >> {
      textAt(index = 0) === 6
    }
    "for the second piece of text, with 'should'" >> {
      textAt(index = 1) === 7
    }
    "for the first example" >> {
      exampleAt(index = 0) === 8
    }
    "for the second example" >> {
      exampleAt(index = 1) === 9
    }
    "for the third piece of text" >> {
      textAt(index = 2) === 11
    }
    "for the 3rd example" >> {
      exampleAt(index = 2) === 12
    }
    "for the 4th example" >> {
      exampleAt(index = 3) === 14
    }
    "for the br element" >> {
      brAt(index = 0) === 10
    }
    "for the end element" >> {
      endAt(index = 0) === 16
    }
  }

  "The FragmentsFragment::def ^(t: String) = fs add Text(t) method must be at line 163. Check the FragmentsBuilder file and fix the Location.scala code" >> {
    val lines = Source.fromFile("src/main/scala/org/specs2/specification/FragmentsBuilder.scala").getLines.toSeq
    // line 163 is index 162
    lines.zipWithIndex.collect { case (line, 162) => line }.headOption must beSome.which { line: String =>
      line must contain("def ^(t: String) = fragments add Text(t)")
    }
  }

  def textAt(index: Int)(implicit spec: SpecificationStructure) = fragmentLine(isSomeText, index)
  def exampleAt(index: Int)(implicit spec: SpecificationStructure) = fragmentLine(isAnExample, index)
  def brAt(index: Int)(implicit spec: SpecificationStructure) = fragmentLine(isABr, index)
  def endAt(index: Int)(implicit spec: SpecificationStructure) = fragmentLine(isAnEnd, index)

  def fragmentLine(selector: PartialFunction[Fragment, Fragment], index: Int)(implicit spec: SpecificationStructure) =
    fragments.collect(selector).apply(index).location.lineNumber

  def fragments(implicit spec: SpecificationStructure) = spec.content.fragments
}
