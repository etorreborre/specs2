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
    ">>" >> {
      "ok" >> ok
      "description" >> { (s: String) => s must ===("description") }
    }
  }

  "A block" >> {
    "with some examples" >> ok
  }

  // see issue #1552: the `should` used to check a String value must not be shadowed
  // by the block DSL `should` used to create a block of examples
  "Checking a String value" >> {
    "with should" >> {
      "actual" should beEqualTo("actual")
    }
  }
