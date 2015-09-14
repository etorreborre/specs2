package org.specs2
package specification
package dsl.mutable

import main.CommandLine

class MutableDslSpec extends org.specs2.mutable.Specification {
  "All those examples" should {
    "compile" >> ok

    "in" >> {
      "ok" in ok
      "command line" in { cl: CommandLine => ok }
      "description" in { s: String => s must_== "description" }
    }
    ">>" >> {
      "ok" >> ok
      "command line" >> { cl: CommandLine => ok }
      "description" >> { s: String => s must_== "description" }
    }
  }

  "A block" >> {
    "with some examples" >> ok
  }
}
