package org.specs2
package reporter

import mutable.Specification
import main.Arguments

class ExamplesIsolationSpec extends Specification with SilentConsoleReporter {
  "An isolated specification must have each example see a fresh state for every local variable in its scope" >> {
    report(spec)(Arguments("isolated")).issues.map(_.message) === Seq()
  }.pendingUntilFixed("This needs further developments")

  val spec = new Specification {
    isolated
    "a system" >> {
      var a = 1
      "should increment variables" >> {
        a = 2
        "ex" >> { a === 2 }
        "ex" >> { a += 1; a === 3}
      }
      "or leave them untouched" >> {
        "ex" >> { a === 1 }
      }
    }
  }
}
