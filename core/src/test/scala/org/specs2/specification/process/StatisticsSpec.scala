package org.specs2
package specification
package process

import Statistics._
import execute._
import org.specs2.concurrent.ExecutionEnv
import specification.core._

class StatisticsSpec(ee: ExecutionEnv) extends Specification { def is = s2"""

 defaultStats sets example = 1 only for examples $e1
 fold adds the fragment result to stats          $e2

"""

  def e1 =
    (defaultStats("ex" ! skipped).examples ==== 1) and
    (defaultStats(step("")).examples ==== 0)

  def e2 = {
    def foldStats(r: Result): Stats =
      Statistics.fold.run(List(Fragment(NoText, Execution.executed(r)))).run(ee)

    Seq(success, failure, pending, skipped) must contain((r: Result) => foldStats(r) === Stats.empty.withResult(r)).forall
  }
}

