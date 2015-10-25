package org.specs2
package specification

import _root_.org.specs2.mutable.{Specification => Spec}
import org.specs2.execute.Result
import org.specs2.main.Arguments
import org.specs2.specification.core.{Env, ContextualSpecificationStructure}
import org.specs2.specification.process.{DefaultSelector, Statistics, Stats, DefaultExecutor}
import user.specification._

class AllExpectationsSpec extends Spec with AllExpectations {

  "A specification with the AllExpectations trait should" >> {
    "evaluate all its expectations" >> {
      executed.hasIssues must beTrue
      executed.expectations === 10
      executed.examples === 4
      executed.failures === 4
      executedIssues.head.message === "'1' is not equal to '2' [AllExpectationsSpecification.scala:8]\n"+
                                      "'1' is not equal to '3' [AllExpectationsSpecification.scala:9]"
    }
    "short-circuit the rest of the example if an expectation fails and uses 'orThrow'" >> {
      executedIssues.map(_.message).toList must containMatch("'51' is not equal to '52'")
      executedIssues.map(_.message) must not containMatch("'13' is not equal to '14'")
    }
    "short-circuit the rest of the example if an expectation fails and uses 'orSkip'" >> {
      executedSuspended.map(_.message).toList must not containMatch("'51' is not equal to '52'")
      executedIssues.map(_.message) must not containMatch("'15' is not equal to '16'")
    }
    "work ok on a specification with selected fragments" >> {
      executedSelected.hasIssues must beTrue
      executedSelected.expectations === 4
      executedSelected.failures === 2
    }
    "work ok on a sequential specification" >> {
      executedSequential.hasIssues must beTrue
      executedSequential.expectations === 10
      executedSequential.failures === 4
    }
    "work ok on a mutable specification with Scopes" >> {
      executedWithScope.hasIssues must beTrue
      executedWithScope.expectations === 3
      executedWithScope.failures === 1
      executedWithScopeIssues.head.message === "'1' is not equal to '2' [AllExpectationsSpecification.scala:31]\n"+
                                               "'1' is not equal to '3' [AllExpectationsSpecification.scala:32]"
    }
    "evaluate an exception" >> {
      executedException.hasIssues must beTrue
      executedException.expectations === 1
      executedException.examples === 1
      executedException.errors === 1
    }
    "evaluate an expression which is not implemented yet" >> {
      executedWithNotImplementedError.hasIssues must beTrue
      executedWithNotImplementedError.expectations === 2
      executedWithNotImplementedError.examples === 1
      executedWithNotImplementedError.errors === 0
      executedWithNotImplementedError.failures === 1
    }
  }

  def stats(spec: ContextualSpecificationStructure)(args: Arguments): Stats = {
    val env = Env(arguments = args)
    try {
      val executed = DefaultExecutor.executeSpecWithoutShutdown(spec.structure(env) |> DefaultSelector.select(env), env)
      Statistics.runStats(executed)
    }
    finally env.shutdown
  }

  def results(spec: ContextualSpecificationStructure)(args: Arguments): IndexedSeq[Result] = {
    val env = Env(arguments = args)
    try DefaultExecutor.executeSpecWithoutShutdown(spec.structure(env), env).fragments.fragments.map(_.executionResult)
    finally env.shutdown
  }

  def issues(spec: ContextualSpecificationStructure)(args: Arguments): IndexedSeq[Result] =
    results(spec)(args).filter(r => r.isError || r.isFailure)

  def suspended(spec: ContextualSpecificationStructure)(args: Arguments): IndexedSeq[Result] =
    results(spec)(args).filter(r => r.isSkipped || r.isPending)

  def executed = stats(new AllExpectationsSpecification)(args())
  def executedIssues = issues(new AllExpectationsSpecification)(args())
  def executedSuspended = suspended(new AllExpectationsSpecification)(args())
  def executedException = stats(new AllExpectationsSpecificationWithException)(args())
  def executedSelected = stats(new AllExpectationsSpecification)(args(ex = "It is"))
  def executedSequential = stats(new AllExpectationsSpecification)(sequential)
  def executedWithScope = stats(new AllExpectationsSpecificationWithScope)(args())
  def executedWithScopeIssues = issues(new AllExpectationsSpecificationWithScope)(args())
  def executedWithNotImplementedError = stats(new AllExpectationsSpecificationWithNotImplementedError)(args())
}

