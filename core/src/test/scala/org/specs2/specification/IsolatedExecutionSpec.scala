package org.specs2
package specification

import execute._
import matcher._
import core._
import process._
import control._
import producer._
import fp.syntax._
import org.specs2.main.Arguments

class IsolatedExecutionSpec(env: Env) extends Spec with ForEachEnv { def is = s2"""

 We want to be able to isolate the execution of examples if isolated == true
 The isolated fragments must run with their own class instance
   for a mutable spec     $e1
   for an acceptance spec $e2
"""

  def e1 = {
    execute(TestIsolatedSpec1) must contain(exactly[Result](Success("example1"), Success("example2")))
  }

  def e2 = {
    execute(TestIsolatedSpec2) must contain(exactly[Result](Success("example1"), Success("example2")))
  }

  def execute(spec: SpecificationStructure) = {
    val env1 = env.copy(arguments = Arguments("isolated"))
    val fragments = spec.structure(env1).fragments

    val results = fragments.update(DefaultExecutor.execute(env1)).contents.runList.flatMap(_.collect { case f if f.isExecutable =>
      f.executionResult
    }.sequence)

    results.run(env1.specs2ExecutionEnv)
  }
}

/**
 * this isolated specification must have all its examples ok because they are properly isolated
 */
object TestIsolatedSpec1 extends org.specs2.mutable.Specification with MustMatchers { isolated
  var n = 0
  "this" should {
    n += 1
    var i = 0
    "create example 1" >> {
      i += 1
      if (i == 1) {
        if (n == 1) Success("example1")
        else Failure(s"example1: n is $n but should be 1")
      } else Failure(s"example1: i is $i but should be 1")
    }
    "create example 2" >> {
      i += 1
      if (i == 1) {
        if (n == 1) Success("example2")
        else Failure(s"example2: n is $n but should be 1")
      } else Failure(s"example2: i is $i but should be 1")
    }
  }
}

object TestIsolatedSpec2 extends org.specs2.Specification with MustMatchers { def is = isolated ^ s2"""

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

