package org.specs2
package specification
package core

import execute.*
import process.*
import scala.concurrent.*, duration.*

class ExecutionSpec(val env: Env) extends Specification with OwnEnv { def is = s2"""

 A link is executed by getting the corresponding specification ref status in the Statistics store
   the Stats is the stats of the spec + specs += 1 $linkExecution

 An execution can be created from a result throwing a FailureException
   it will then create a failed result $withFailureException

 An execution can be created from a result throwing a fatal exception
   it will then throw an ExecutionException exception $withFatalException

"""

  def linkExecution =
    val store = StatisticsRepositoryCreation.memory
    val env1 = ownEnv.setStatisticRepository(store)
    val stats =  Stats(specs = 2, failures = 1, examples = 1)
    store.storeStatistics(getClass.getName, stats).runOption

    Execution.specificationStats(getClass.getName).result(env1) `must` beLike {
      case DecoratedResult(t, r) =>
        t.asInstanceOf[Matchable] match
          case s: Stats =>
            (s `must` ===(Stats(specs = 3, failures = 1, examples = 1))) `and`
              (r.isSuccess `must` beFalse)
          case other =>
            ko("expected some stats, got "+other)
    }

  def withFailureException =
    val failure = Failure("ko")
    Execution.withEnv(_ => {throw new FailureException(failure); success}).result(env) === failure

  def withFatalException =
    Execution.withEnv(_ => {throw new java.lang.NoSuchMethodError("boom"); success}).result(env) `must` throwAn[ExecutionException]

  /**
   * HELPERS
   */

  extension (e: Execution)
    def result(env: Env): Result =
      Await.result(e.startExecution(env).executedResult.runFuture(env.executionEnv), 10.seconds).result
}
