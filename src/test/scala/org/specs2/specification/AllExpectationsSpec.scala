package org.specs2
package specification

import reporter._
import user.specification.AllExpectationsSpecification

class AllExpectationsSpec extends mutable.Specification with AllExpectations {
  sequential
  "A specification with the AllExpectations trait should" >> {
    "evaluate all its expectations" >> {
      executed.hasIssues must beTrue
      executed.stats.expectations === 8
      executed.stats.examples === 4
      executed.stats.failures === 3
      executed.issues.head.message === "'1' is not equal to '2' [AllExpectationsSpecification.scala:8]\n"+
                                       "'1' is not equal to '3' [AllExpectationsSpecification.scala:9]"
    }
    "short-circuit the rest of the example if an expectation fails and uses 'orThrow'" >> {
      executed.issues.map(_.message) must not containMatch("'13' is not equal to '14'")
    }
    "short-circuit the rest of the example if an expectation fails and uses 'orSkip'" >> {
      executed.suspended.map(_.message) must containMatch("'51' is not equal to '52'")
      executed.issues.map(_.message) must not containMatch("'15' is not equal to '16'")
    }
  }

  def executed = SilentConsoleReporter.report(new AllExpectationsSpecification)(args())
}

