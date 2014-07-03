package org.specs2
package specification

import process._
import Statistics._
import scalaz.stream.Process
import matcher._
import scalaz.concurrent.Task
import DefaultExecutor._
import dsl.FragmentsDsl

class StatsSpec extends Specification { import stats1._; def is = s2"""

 Statitistics can be computed for a stream of fragments
  1 success            $e1
  1 success, 1 failure $e2

"""
}

import dsl1._
import ValueChecks._

object stats1 extends MustMatchers with StandardMatchResults with MatchersImplicits {
  import ProcessMatchers._

  def e1 = {
    val p = Process("ex1" ! ok).toSource |> executeFragments1 |> statsProcess
    p must haveLast(Stats(examples = 1, expectations = 1, successes = 1))
  }

  def e2 = {
    val p = Process("ex1" ! ok, "ex2" ! ko).toSource |> executeFragments1 |> statsProcess
    p must haveLast(Stats(examples = 2, expectations = 2, successes = 1, failures = 1))
  }

}