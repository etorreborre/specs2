package org.specs2
package io

import control._
import org.specs2.matcher.TypedEqual
import specification.core._
import user.io.{LocationSpecification, LocationUnitSpecification}
import Fragment._
import org.specs2.concurrent.ExecutionEnv

class LocationSpec extends org.specs2.mutable.Spec with TypedEqual:
  given ee as ExecutionEnv = Env().executionEnv

    "A unit specification must have correct locations for its fragments" >> {
      implicit def spec: LocationUnitSpecification = new LocationUnitSpecification(ee)

      "for the first piece of text, with 'should'" >> {
        textAt(index = 0)(spec) ==== 17
      }
      "for the first example, with 'in'" >> {
        exampleAt(index = 0) ==== 15
      }
      "for the second example, with 'in'" >> {
        exampleAt(index = 1) ==== 16
      }
      "for the second piece of text, with '>>'" >> {
        textAt(index = 1) ==== 24
      }
      "for the 3rd example, with '>>'" >> {
        exampleAt(index = 2) ==== 21
      }
      "for the 4th example, with '>>'" >> {
        exampleAt(index = 3) ==== 23
      }
    }

  "An acceptance specification must have correct locations for its fragments" >> {
    implicit def spec: LocationSpecification = new LocationSpecification(ee)

    "for the first piece of text, 'presentation''" >> {
      textAt(index = 0) ==== 7
    }
    "for the second piece of text, with 'should'" >> {
      textAt(index = 1) ==== 10
    }
    "for the first example" >> {
      exampleAt(index = 0) ==== 10
    }
    "for the second example" >> {
      exampleAt(index = 1) ==== 11
    }
    "for the third piece of text" >> {
      textAt(index = 2) ==== 11
    }
    "for the 3rd example" >> {
      exampleAt(index = 2) ==== 14
    }
    "for the 4th example" >> {
      exampleAt(index = 3) ==== 15
    }
  }

  def textAt(index: Int)(implicit spec: WithFragments) = fragmentLine(isText, index)
  def exampleAt(index: Int)(implicit spec: WithFragments) = fragmentLine(isExample, index)
  def brAt(index: Int)(implicit spec: WithFragments) = fragmentLine(isFormatting, index)

  def fragmentLine(selector: Function[Fragment, Boolean], index: Int)(implicit spec: WithFragments) =

    val filter = StackTraceFilter(trace =>
      !Seq("scala.",
           "org.specs2.Specification",
           "org.specs2.specification.",
           "org.specs2.mutable.").exists(trace.getClassName.startsWith))

    val fragmentLocation = fragments(spec).filter(selector).apply(index).location

    fragmentLocation.lineNumber

  def fragments(implicit spec: WithFragments): List[Fragment] =
    spec.fragmentsList

  step(ee.shutdown())

trait WithFragments:
  def fragmentsList: List[Fragment]
