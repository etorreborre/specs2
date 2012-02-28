package org.specs2
package specification

import reporter._
import user.specification.AllExpectationsSpecification

class AllExpectationsSpec extends mutable.Specification with AllExpectations {
  "A specification with the AllExpectations trait should" >> {
    "evaluate all its expectations" >> {
      executed.hasIssues must beTrue
      executed.stats.expectations === 6
      executed.stats.examples === 2
      executed.stats.failures === 2
      executed.issues.head.message === "'1' is not equal to '2' [AllExpectationsSpecification.scala:8]\n"+
                                       " '1' is not equal to '3' [AllExpectationsSpecification.scala:9]"
    }
  }

  def executed = SilentConsoleReporter.report(new AllExpectationsSpecification)(args())
}

