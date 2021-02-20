package org.specs2
package specification

import _root_.org.specs2.mutable.{Specification as Spec}
import org.specs2.execute.Result
import org.specs2.main.Arguments
import org.specs2.specification.core.{SpecificationStructure, Env, OwnEnv}
import org.specs2.specification.process.*
import user.specification.*
import fp.syntax.*

class AllExpectationsSpec(val env: Env) extends Spec with OwnEnv:

  "A specification with the AllExpectations trait should" >> {
    "evaluate all its expectations" >> {
      executed.hasIssues `must` beTrue
      executed.expectations === 8
      executed.examples === 3
      executed.failures === 3
      executedIssues.head.message === "1 != 2 [AllExpectationsSpecification.scala:8]\n"+
                                      "1 != 3 [AllExpectationsSpecification.scala:9]"
    }
    "short-circuit the rest of the example if an expectation fails and uses 'orThrow'" >> {
      executedIssues.map(_.message) `must` containMatch("51 != 52")
      executedIssues.map(_.message) `must` not(containMatch("13 != 14"))
    }
    "short-circuit the rest of the example if an expectation fails and uses 'orSkip'" >> {
      executedSuspended.map(_.message) `must` not(containMatch("'51' is not equal to '52'"))
      executedIssues.map(_.message) `must` not(containMatch("'15' is not equal to '16'"))
    }
    "work ok on a specification with selected fragments" >> {
      executedSelected.hasIssues `must` beTrue
      executedSelected.expectations === 2
      executedSelected.failures === 1
    }
    "evaluate an exception" >> {
      executedException.hasIssues `must` beTrue
      executedException.expectations === 1
      executedException.examples === 1
      executedException.errors === 1
    }
    "evaluate an expression which is not implemented yet" >> {
      executedWithNotImplementedError.hasIssues `must` beTrue
      executedWithNotImplementedError.expectations === 2
      executedWithNotImplementedError.examples === 1
      executedWithNotImplementedError.errors === 0
      executedWithNotImplementedError.failures === 1
    }
    "evaluate an expression which returns skipped" >> {
      executedWithSkipped.hasIssues `must` beTrue
      executedWithSkipped.expectations === 3
      executedWithSkipped.examples === 2
      executedWithSkipped.errors === 0
      // the first example registers a final result = Skipped
      executedWithSkipped.skipped === 1
      // the second example registers a final result = Failure
      // (the skipped value is "absorbed by the failure")
      executedWithSkipped.failures === 1
    }
  }

  def stats(spec: SpecificationStructure)(args: Arguments): Stats =
    // all the executions need to be sequential
    val executed = DefaultExecutor.executeSpec(spec.structure |> DefaultSelector(Arguments()).select(args), ownEnv)
    Statistics.runStats(executed)(ownEnv.executionEnv)

  def results(spec: SpecificationStructure)(args: Arguments): List[Result] =
    // all the executions need to be sequential
    val env1 = ownEnv.setArguments(args <| sequential)
    DefaultExecutor.executeSpec(spec.structure, env1).fragments.fragments.
      flatMap(_.traverse(_.executionResult)).run(env1.executionEnv)

  def issues(spec: SpecificationStructure)(args: Arguments): List[Result] =
    results(spec)(args).filter(r => r.isError || r.isFailure)

  def suspended(spec: SpecificationStructure)(args: Arguments): List[Result] =
    results(spec)(args).filter(r => r.isSkipped || r.isPending)

  def executed = stats(new AllExpectationsSpecification)(args())
  def executedIssues = issues(new AllExpectationsSpecification)(args())
  def executedSuspended = suspended(new AllExpectationsSpecification)(args())
  def executedException = stats(new AllExpectationsSpecificationWithException)(args())
  def executedSelected = stats(new AllExpectationsSpecification)(args(ex = "It is"))
  def executedWithScope = stats(new AllExpectationsSpecificationWithScope)(args())
  def executedWithScopeIssues = issues(new AllExpectationsSpecificationWithScope)(args())
  def executedWithNotImplementedError = stats(new AllExpectationsSpecificationWithNotImplementedError)(args())
  def executedWithSkipped = stats(new AllExpectationsSpecificationWithSkipped)(args())
