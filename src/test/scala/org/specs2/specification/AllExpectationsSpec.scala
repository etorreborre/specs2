package org.specs2
package specification

import reporter._

class AllExpectationsSpec extends mutable.Specification with AllExpectations {
  "A specification with the AllExpectations trait should" >> {
    "evaluate all its expectations" >> {
      executed.hasIssues must beTrue
      executed.stats.expectations === 6
      executed.stats.failures === 2
    }
  }

  def executed = SilentConsoleReporter.report(new AllTheExpectations)(args())
}

class AllTheExpectations extends mutable.Specification with AllExpectations {
  "In this example all the expectations are evaluated" >> {
    1 === 2
    1 === 3
    1 === 1
  }
  "There is no collision with this example" >> {
    10 === 11
    12 === 31
    13 === 13
  }
}
