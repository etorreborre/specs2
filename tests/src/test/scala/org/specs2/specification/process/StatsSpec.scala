package org.specs2
package specification
package process

import execute._
import DefaultExecutor._
import Statistics._
import control._
import org.specs2.specification.core.{Env, OwnEnv}
import producer._
import ExecuteActions._

class StatsSpec(val env: Env) extends Specification with OwnEnv { def is = s2"""

 Statitistics can be computed for a stream of fragments
  1 success            $e1
  1 success, 1 failure $e2

 Statistics can be created from a result
  a simple result                            $r1
  a decorated result coming from store stats $r2

"""

  def e1 = {
    val p = emitAsync("ex1" ! ok) |> executeFragments1(ownEnv) |> statsProcess
    runLast(p) must beSome(Stats(examples = 1, expectations = 1, successes = 1))
  }

  def e2 = {
    val p = emitAsync("ex1" ! ok, "ex2" ! ko) |> executeFragments1(ownEnv) |> statsProcess
    runLast(p) must beSome(Stats(examples = 2, expectations = 2, successes = 1, failures = 1))
  }

  /**
   * HELPERS
   */

  def runLast[A](p: AsyncStream[A]): Option[A] =
    p.runList.runOption(ownEnv.specs2ExecutionEnv).map(_.lastOption).flatten

  def r1 =
    Stats(failure.setExpectationsNb(3)) === Stats(failures = 1, examples = 1, expectations = 3)

  def r2 =
    Stats(DecoratedResult(Stats(failure.setExpectationsNb(3)), failure.setExpectationsNb(3))) ===
      Stats(failures = 1, examples = 1, expectations = 3)

}
