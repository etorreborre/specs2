package org.specs2
package specification
package core

import execute._
import process._
import control._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.matcher.{ActionMatchers, ResultMatchers}

import scala.concurrent.duration._
import scala.concurrent.Future
import Actions._

class ExecutionSpec(implicit ee: ExecutionEnv) extends Specification with ActionMatchers with ResultMatchers { def is = s2"""

 A link is executed by getting the corresponding specification ref status in the Statistics store
   the Stats is the stats of the spec + specs += 1 $linkExecution

 An execution can be timed-out $withTimeout

"""

  def linkExecution = { env1: Env =>
    val store = StatisticsRepository.memory
    val env = env1.setStatisticRepository(store)
    val stats =  Stats(specs = 2, failures = 1, examples = 1)
    store.storeStatistics(getClass.getName, stats).runOption

    timedFuture(Execution.specificationStats(getClass.getName).startExecution(env).executionResult) must beOk(beLike[Result] {
      case DecoratedResult(s: Stats, r) =>
        (s must_== Stats(specs = 3, failures = 1, examples = 1)) and
        (r.isSuccess must beFalse)
    })

  }

  def withTimeout = { env: Env =>
    val execution = Execution.withEnvAsync(_ => Future {
      Thread.sleep(1000)
      success }).setTimeout(100.millis)
    timedFuture(execution.startExecution(env).executionResult) must beOk(beSkipped[Result])
  }
}
