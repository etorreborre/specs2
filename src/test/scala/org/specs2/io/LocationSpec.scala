package org.specs2
package io

import mutable.Specification
import user.io.{LocationSpecification, LocationUnitSpecification}
import specification.Fragments._
import specification.{SpecificationStructure, Fragment}

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

    "for the first piece of text, with 'should'" >> {
      textAt(index = 0) === 6
    }
    "for the first example" >> {
      exampleAt(index = 0) === 7
    }
    "for the second example" >> {
      exampleAt(index = 1) === 8
    }
    "for the second piece of text" >> {
      textAt(index = 1) === 10
    }
    "for the 3rd example" >> {
      exampleAt(index = 2) === 11
    }
    "for the 4th example" >> {
      exampleAt(index = 3) === 13
    }
  }

  def textAt(index: Int)(implicit spec: SpecificationStructure) = fragmentLine(isSomeText, index)
  def exampleAt(index: Int)(implicit spec: SpecificationStructure) = fragmentLine(isAnExample, index)

  def fragmentLine(selector: PartialFunction[Fragment, Fragment], index: Int)(implicit spec: SpecificationStructure) =
    fragments.collect(selector).apply(index).location.lineNumber

  def fragments(implicit spec: SpecificationStructure) = spec.content.fragments
}
