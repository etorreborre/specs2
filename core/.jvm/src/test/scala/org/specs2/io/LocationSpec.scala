package org.specs2
package io

import control.*
import org.specs2.matcher.TypedEqual
import specification.core.*
import user.io.{LocationSpecification, LocationUnitSpecification}
import Fragment.*
import org.specs2.concurrent.ExecutionEnv

class LocationSpec extends org.specs2.mutable.Spec with TypedEqual:
  given ee: ExecutionEnv = Env().executionEnv

  "A unit specification must have correct locations for its fragments" >> {
    given spec: LocationUnitSpecification = new LocationUnitSpecification(ee)

    "for the first piece of text, with 'should'" >> {
      textAt(index = 0) === 16
    }
    "for the first example, with 'in'" >> {
      exampleAt(index = 0) === 14
    }
    "for the second example, with 'in'" >> {
      exampleAt(index = 1) === 15
    }
    "for the second piece of text, with '>>'" >> {
      textAt(index = 1) === 23
    }
    "for the 3rd example, with '>>'" >> {
      exampleAt(index = 2) === 20
    }
    "for the 4th example, with '>>'" >> {
      exampleAt(index = 3) === 22
    }
  }

  "An acceptance specification must have correct locations for its fragments" >> {
    given LocationSpecification = new LocationSpecification(ee)

    "for the first piece of text, 'presentation''" >> {
      textAt(index = 0) === 8
    }
    "for the second piece of text, with 'should'" >> {
      textAt(index = 1) === 11
    }
    "for the first example" >> {
      exampleAt(index = 0) === 11
    }
    "for the second example" >> {
      exampleAt(index = 1) === 12
    }
    "for the third piece of text" >> {
      textAt(index = 2) === 12
    }
    "for the 3rd example" >> {
      exampleAt(index = 2) === 15
    }
    "for the 4th example" >> {
      exampleAt(index = 3) === 16
    }
  }

  def textAt(index: Int)(using spec: WithFragments) = fragmentLine(isText, index)
  def exampleAt(index: Int)(using spec: WithFragments) = fragmentLine(isExample, index)
  def brAt(index: Int)(using spec: WithFragments) = fragmentLine(isFormatting, index)

  def fragmentLine(selector: Function[Fragment, Boolean], index: Int)(using spec: WithFragments) =

    val filter = StackTraceFilter(trace =>
      !Seq("scala.", "org.specs2.Specification", "org.specs2.specification.", "org.specs2.mutable.").exists(
        trace.getClassName.startsWith
      )
    )

    val fragmentLocation = fragments(using spec).filter(selector).apply(index).location

    fragmentLocation.lineNumber

  def fragments(using spec: WithFragments): List[Fragment] =
    spec.fragmentsList

  step(ee.shutdown())

trait WithFragments:
  def fragmentsList: List[Fragment]
