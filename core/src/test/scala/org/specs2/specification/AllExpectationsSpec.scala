package org.specs2
package specification

import _root_.org.specs2.mutable.{Specification => Spec}
import reporter._
import user.specification.{AllExpectationsSpecificationWithException, AllExpectationsSpecificationWithScope, AllExpectationsSpecification}

class AllExpectationsSpec extends Spec with AllExpectations {

  "A specification with the AllExpectations trait should" >> {
    "evaluate all its expectations" >> {
      executed.hasIssues must beTrue
      executed.stats.expectations === 10
      executed.stats.examples === 4
      executed.stats.failures === 4
      executed.issues.head.message === "'1' is not equal to '2' [AllExpectationsSpecification.scala:8]\n"+
                                       "'1' is not equal to '3' [AllExpectationsSpecification.scala:9]"
    }
    "short-circuit the rest of the example if an expectation fails and uses 'orThrow'" >> {
      executed.issues.map(_.message).toList must containMatch("'51' is not equal to '52'")
      executed.issues.map(_.message) must not containMatch("'13' is not equal to '14'")
    }
    "short-circuit the rest of the example if an expectation fails and uses 'orSkip'" >> {
      executed.suspended.map(_.message).toList must not containMatch("'51' is not equal to '52'")
      executed.issues.map(_.message) must not containMatch("'15' is not equal to '16'")
    }
    "work ok on a specification with selected fragments" >> {
      executedSelected.hasIssues must beTrue
      executedSelected.stats.expectations === 4
      executedSelected.stats.failures === 2
    }
    "work ok on a sequential specification" >> {
      executedSequential.hasIssues must beTrue
      executedSequential.stats.expectations === 10
      executedSequential.stats.failures === 4
    }
    "work ok on a mutable specification with Scopes" >> {
      executedWithScope.hasIssues must beTrue
      executedWithScope.stats.expectations === 3
      executedWithScope.stats.failures === 1
      executedWithScope.issues.head.message === "'1' is not equal to '2' [AllExpectationsSpecification.scala:31]\n"+
                                                "'1' is not equal to '3' [AllExpectationsSpecification.scala:32]"
    }
    "evaluate an exception" >> {
      executedException.hasIssues must beTrue
      executedException.stats.expectations === 1
      executedException.stats.examples === 1
      executedException.stats.errors === 1
    }
  }

  def executed = SilentConsoleReporter.report(new AllExpectationsSpecification)(args())
  def executedException = SilentConsoleReporter.report(new AllExpectationsSpecificationWithException)(args())
  def executedSelected = SilentConsoleReporter.report(new AllExpectationsSpecification)(args(ex = "It is"))
  def executedSequential = SilentConsoleReporter.report(new AllExpectationsSpecification)(sequential)
  def executedWithScope = SilentConsoleReporter.report(new AllExpectationsSpecificationWithScope)(args())
}

