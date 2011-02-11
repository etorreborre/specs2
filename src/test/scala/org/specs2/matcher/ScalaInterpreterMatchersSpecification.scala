package org.specs2
package matcher
import org.specs2.mutable.SpecificationWithJUnit

class ScalaInterpreterMatchersSpecification extends SpecificationWithJUnit with ScalaInterpreterMatchers {

  "A script" can {
    "be interpreted" in {
      """
      1 + 1
      """ >| "2"
    }
    "fail when interpreted" in {
      ("""
       hello
       """ >| "2").message must not =~("error: not found: value hello")
    }
  }

}