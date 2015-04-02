package org.specs2
package specification
package core

import execute._
import process._
import control._

class ExecutionSpec extends Specification { def is = s2"""

 A link is executed by getting the corresponding specification ref status in the Statistics store
   the Stats is the stats of the spec + specs += 1 $linkExecution

"""

  def linkExecution = { env1: Env =>
    val store = StatisticsRepository.memory
    val env = env1.setStatisticRepository(store)
    val stats =  Stats(specs = 2, failures = 1, examples = 1)
    store.storeStatistics(getClass.getName, stats).runOption

    Execution.specificationStats(getClass.getName).execute(env).result must beLike {
      case DecoratedResult(s: Stats, r) =>
        (s must_== Stats(specs = 3, failures = 1, examples = 1)) and
        (r.isSuccess must beFalse)
    }

  }
}
