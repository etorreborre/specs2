package org.specs2.specification.process

import org.specs2.Specification
import org.specs2.concurrent.ExecutionEnv
import org.specs2.control.ExecuteActions._
import org.specs2.execute.Result
import org.specs2.specification.core.{Execution, Fragment, NoText}
import org.specs2.specification.process.Statistics._

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

