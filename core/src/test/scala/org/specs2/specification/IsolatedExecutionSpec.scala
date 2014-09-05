package org.specs2
package specification

import execute._
import matcher._
import main.Arguments
import core._
import process._

class IsolatedExecutionSpec extends Spec with ForEachEnv { def is = s2"""

 We want to be able to isolate the execution of examples if isolated == true
 The isolated fragments must run with their own class instance
   for a mutable spec     $e1
   for an acceptance spec $e2
"""

  def e1 = {
    val spec = new TestIsolatedSpec1
    val executed = execute(spec)
    val results = executed.collect { case f if f.isExecutable => f.execution.result }
    results must contain(exactly[Result](Success("example1"), Success("example2")))
  }

  def e2 = {
    val spec = new TestIsolatedSpec2
    val executed = execute(spec)

    val results = executed.collect { case f if f.isExecutable => f.execution.result }
    results must contain(exactly[Result](Success("example1"), Success("example2")))
  }

  def execute(spec: SpecificationStructure) = {
    val env = Env()
    val fragments = spec.structure(env).fragments

    try Fragments(DefaultExecutor.execute(env)(fragments.contents)).fragments
    finally env.shutdown
  }
}

/**
 * this isolated specification must have all its examples ok because they are properly isolated
 */
class TestIsolatedSpec1 extends org.specs2.mutable.Specification with MustMatchers { isolated
  "this" should {
    var i = 0
    "create example 1" >> {
      i += 1
      if (i == 1) Success("example1") else Failure(s"example1: i is $i but should be 1")
    }
    "create example 2" >> {
      i += 1
      if (i == 1) Success("example2") else Failure(s"example2: i is $i but should be 1") }
  }
}

class TestIsolatedSpec2 extends org.specs2.Specification with MustMatchers { def is = isolated ^
  s2"""
  this should
    create example 1 $e1
    create example 2 $e2
  """

  def e1 = {
    i += 1
    if (i == 1) Success("example1") else Failure(s"example1: i is $i but should be 1")
  }

  def e2 = {
    i += 1
    if (i == 1) Success("example2") else Failure(s"example2: i is $i but should be 1")
  }

  var i = 0
}

