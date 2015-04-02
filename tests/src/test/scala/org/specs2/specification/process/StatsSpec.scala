package org.specs2
package specification
package process

import execute._
import matcher.ProcessMatchers
import scalaz.stream.Process
import DefaultExecutor._
import Statistics._

class StatsSpec extends Specification with ProcessMatchers { def is = s2"""

 Statitistics can be computed for a stream of fragments
  1 success            $e1
  1 success, 1 failure $e2

 Statistics can be created from a result
  a simple result                            $r1
  a decorated result coming from store stats $r2

"""

  def e1 = {
    val p = Process("ex1" ! ok).toSource |> executeFragments1 |> statsProcess
    p must returnLast(Stats(examples = 1, expectations = 1, successes = 1))
  }

  def e2 = {
    val p = Process("ex1" ! ok, "ex2" ! ko).toSource |> executeFragments1 |> statsProcess
    p must returnLast(Stats(examples = 2, expectations = 2, successes = 1, failures = 1))
  }

  def r1 =
    Stats(failure.setExpectationsNb(3)) === Stats(failures = 1, examples = 1, expectations = 3)

  def r2 =
    Stats(DecoratedResult(Stats(failure.setExpectationsNb(3)), failure.setExpectationsNb(3))) ===
      Stats(failures = 1, examples = 1, expectations = 3)

}