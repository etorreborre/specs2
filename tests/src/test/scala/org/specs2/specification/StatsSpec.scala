package org.specs2
package specification

import matcher._
import org.specs2.specification.dsl.ExampleDsl
import specification.process.Stats
import scalaz.stream.Process

class StatsSpec extends Spec { def is = s2"""

 Statitistics can be computed for a stream of fragments
  1 success            ${stats1.e1}
  1 success, 1 failure ${stats1.e2}

"""
}

import process.DefaultExecutor._
import process.Statistics._

object stats1 extends MustMatchers with StandardMatchResults with MatchersImplicits with ProcessMatchers with ExampleDsl {

  def e1 = {
    val p = Process("ex1" ! ok).toSource |> executeFragments1 |> statsProcess
    p must returnLast(Stats(examples = 1, expectations = 1, successes = 1))
  }

  def e2 = {
    val p = Process("ex1" ! ok, "ex2" ! ko).toSource |> executeFragments1 |> statsProcess
    p must returnLast(Stats(examples = 2, expectations = 2, successes = 1, failures = 1))
  }

}