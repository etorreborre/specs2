package org.specs2
package io

import mutable.Specification
import org.specs2.specification.core.{Fragment, SpecificationStructure}
import specification._
import user.io.{LocationSpecification, LocationUnitSpecification}
import Fragment._

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
  }

  def textAt(index: Int)(implicit spec: SpecificationStructure) = fragmentLine(isText, index)
  def exampleAt(index: Int)(implicit spec: SpecificationStructure) = fragmentLine(isExample, index)
  def brAt(index: Int)(implicit spec: SpecificationStructure) = fragmentLine(isFormatting, index)

  def fragmentLine(selector: Function[Fragment, Boolean], index: Int)(implicit spec: SpecificationStructure) =
    fragments(spec).filter(selector).apply(index).location.lineNumber(arguments.traceFilter).getOrElse(-1)

  def fragments(implicit spec: SpecificationStructure) = spec.is.fragments.fragments
}
