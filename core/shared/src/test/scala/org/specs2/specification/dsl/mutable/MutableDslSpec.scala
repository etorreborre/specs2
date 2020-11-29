package org.specs2
package specification
package dsl.mutable

class MutableDslSpec extends org.specs2.mutable.Specification:
  "All those examples" should {
    "compile" >> ok

    "in" >> {
      "ok" in ok
      "description" in { (s: String) => s must ===("description") }
    }
    describeBlock(">>") >> {
      "ok" >> ok
      "description" >> { (s: String) => s must ===("description") }
    }
  }

  "A block" >> {
    "with some examples" >> ok
  }
