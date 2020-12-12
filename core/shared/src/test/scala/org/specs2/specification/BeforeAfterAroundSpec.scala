package org.specs2
package specification

import io.StringOutput
import execute._
import org.specs2.specification.core.{Env, SpecificationStructure, Fragment}
import org.specs2.specification.process.DefaultExecutor
import _root_.org.specs2.mutable.{Specification => Spec}
import fp.syntax._

class BeforeAfterAroundSpec extends Specification { def is = s2"""

 The `Before/After/Around Example` traits are used to automatically insert contexts around examples bodies

 a spec can define a Before context that is used for each example
   in a mutable spec $mutableBefore
   also in an acceptance spec $acceptanceBefore

 a spec can define an After context that is used for each example $afterContext
 a spec can define an Around context that is used for each example $aroundContext

"""

  def mutableBefore = executeContains(
    new Spec with BeforeEach with StringOutput {
      def before = step(println("before"))
      "ex1" ! success
    }, "before")

  def acceptanceBefore = executeContains(
    new Specification with BeforeEach with StringOutput {
      def before = step(println("before"))
      def is = "ex1" ! success
    }, "before")

  def afterContext = executeContains(
    new Spec with AfterEach with StringOutput {
      def after = step(println("after"))
      "ex1" ! success
    },"after")

  def aroundContext = executeContains(
    new Spec with StringOutput {
      override def flatMap(f: Fragment) = { step(println("around")) ^ f }
      "ex1" ! success
    },"around")


  def executeContains(s: SpecificationStructure with StringOutput, messages: String*) =
    val env = Env()
    try
      DefaultExecutor.executeFragments(s.is.fragments)(env).traverse(_.executionResult).run(env.executionEnv)
      s.messages must contain(allOf(messages:_*)).inOrder
    finally env.shutdown()

}
