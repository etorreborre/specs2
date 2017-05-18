package org.specs2
package io

import control._
import org.specs2.matcher.TypedEqual
import specification.core._
import user.io.{LocationSpecification, LocationUnitSpecification}
import Fragment._
import org.specs2.concurrent.ExecutionEnv

class LocationSpec extends org.specs2.mutable.Spec with TypedEqual {
  implicit lazy val ee: ExecutionEnv = Env().executionEnv

  "A unit specification must have correct locations for its fragments" >> {
    implicit def spec = new LocationUnitSpecification(ee)

    "for the first piece of text, with 'should'" >> {
      textAt(index = 0)(spec) === Some(12)
    }
    "for the first example, with 'in'" >> {
      exampleAt(index = 0) === Some(13)
    }
    "for the second example, with 'in'" >> {
      exampleAt(index = 1) === Some(14)
    }
    "for the second piece of text, with '>>'" >> {
      textAt(index = 1) === Some(17)
    }
    "for the 3rd example, with '>>'" >> {
      exampleAt(index = 2) === Some(18)
    }
    "for the 4th example, with '>>'" >> {
      exampleAt(index = 3) === Some(20)
    }
  }

  "An acceptance specification must have correct locations for its fragments" >> {
    implicit def spec = new LocationSpecification(ee)

    "for the first piece of text, 'presentation''" >> {
      textAt(index = 0) === Some(7)
    }
    "for the second piece of text, with 'should'" >> {
      textAt(index = 1) === Some(10)
    }
    "for the first example" >> {
      exampleAt(index = 0) === Some(10)
    }
    "for the second example" >> {
      exampleAt(index = 1) === Some(11)
    }
    "for the third piece of text" >> {
      textAt(index = 2) === Some(11)
    }
    "for the 3rd example" >> {
      exampleAt(index = 2) === Some(14)
    }
    "for the 4th example" >> {
      exampleAt(index = 3) === Some(15)
    }
  }

  def textAt(index: Int)(implicit spec: WithFragments) = fragmentLine(isText, index)
  def exampleAt(index: Int)(implicit spec: WithFragments) = fragmentLine(isExample, index)
  def brAt(index: Int)(implicit spec: WithFragments) = fragmentLine(isFormatting, index)

  def fragmentLine(selector: Function[Fragment, Boolean], index: Int)(implicit spec: WithFragments) = {

    val filter = StackTraceFilter(trace =>
      !Seq("scala.",
           "org.specs2.Specification",
           "org.specs2.specification.",
           "org.specs2.mutable.").exists(trace.getClassName.startsWith))

    val fragmentLocation = fragments(spec).filter(selector).apply(index).location

    fragmentLocation.filter(filter).lineNumber(filter)
  }

  def fragments(implicit spec: WithFragments): List[Fragment] =
    spec.fragmentsList

  step(ee.shutdown)
}

trait WithFragments {
  def fragmentsList: List[Fragment]
}
