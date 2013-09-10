package org.specs2
package reporter

import mutable.Specification
import main.Arguments

class ExamplesIsolationSpec extends Specification with SilentConsoleReporter {
  "An isolated specification must have each example see a fresh state for every local variable in its scope" >> {
    report(spec)(Arguments("isolated")).issues.toList.map(i => (i.s, i.message)) must beEmpty
  }

  val spec = new SpecificationWithLocalVariables
}

class SpecificationWithLocalVariables extends Specification {
  isolated
  sequential

  "a system" >> {
    var a = 1
    "should increment variables" >> {
      a = 2
      "ex1" >> { a === 2 }
      "ex2" >> { a += 1; a === 3 }
    }
    "or leave them untouched" >> {
      "ex3" >> { a === 1 }
    }
  }
}