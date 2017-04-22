package org.specs2
package specification

import io.StringOutput
import execute._
import org.specs2.specification.core.{Env, SpecificationStructure}
import org.specs2.specification.process.DefaultExecutor
import _root_.org.specs2.mutable.{Specification => Spec}
import fp.syntax._

class BeforeAfterAroundSpec(env: Env) extends Specification with Grouped { def is = s2"""

 The `Before/After/Around Example` traits are used to automatically insert contexts around examples bodies            
 a spec can define a Before context that is used for each example                                                     
   in a mutable spec                                                                                          ${g1.e1}
   also in an acceptance spec                                                                                 ${g1.e2}

 a spec can define an After context that is used for each example                                             ${g2.e1}
 a spec can define an Around context that is used for each example                                            ${g2.e2}
                                                                                                              """

  "before" - new g1 {
    e1 := executeContains(
      new Spec with BeforeEach with StringOutput {
        def before { println("before") }
        "ex1" ! success
      }, "before")

    e2 := executeContains(
      new Specification with BeforeEach with StringOutput {
        def before { println("before") }
        def is = "ex1" ! success
      }, "before")

  }

  "other" - new g2 {
    e1 := executeContains(
    new Spec with AfterEach with StringOutput {
      def after { println("after") }
      "ex1" ! success
    },"after")

    e2 := executeContains(
      new Spec with AroundEach with StringOutput {
        def around[R : AsResult](r: =>R) = { println("around"); AsResult(r) }
        "ex1" ! success
      },"around")

  }

  def executeContains(s: SpecificationStructure with StringOutput, messages: String*) = {
    DefaultExecutor.executeSeq(s.is.fragments.fragments)(env).traverse(_.executionResult).run(env.executionEnv)
    s.messages must contain(allOf(messages:_*)).inOrder
  }

}
