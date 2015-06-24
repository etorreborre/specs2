package org.specs2
package specification
package process

import Statistics._
import execute._
import specification.core._
import scalaz._, Scalaz._

class StatisticsSpec extends Specification { def is = s2"""

 defaultStats set example = 1 only for examples $e1
 fold adds the fragment result to stats $e2

"""

  def e1 =
    (defaultStats("ex" ! skipped).examples ==== 1) and
    (defaultStats(step("")).examples ==== 0)

  def e2 = {
    def foldStats(r: Result): Stats =
      Statistics.fold(Fragment(NoText, Execution.executed(r)), Stats.empty)

    Seq(success, failure, pending, skipped) must contain((r: Result) => foldStats(r) === Stats.empty.withResult(r)).forall
  }
}

