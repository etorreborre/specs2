package org.specs2
package mutable

import io._
import specification.{Example, Groups, SpecStart, FragmentExecution}
import execute.{Result, AsResult, FailureException}

class MutableSpecificationSpec extends Specification with Groups { s2"""

A Specification can be written in the specs style using should/in blocks.
This works by building the fragments and mutating a local variable

The following examples specify the functionalities for such a mutable specification.

  Fragments creation
    a `should` block creates a text fragment                                                         ${g1().e1}
    every example in a should block creates examples fragments following the `should` text           ${g1().e2}
    an action can be created with the `action` keyword                                               ${g1().e3}
    arguments can be created with the `args` keyword                                                 ${g1().e4}
    examples can be nested                                                                           ${g1().e5}
    should expectations can be used                                                                  ${g1().e6}
    a text fragment can start a block with                                                           ${g1().e7}
    foreach can be used to create a block of examples with the `examplesBlock` method                ${g1().e8}
    foreach can be used inside an example with the `Result.unit` method                              ${g1().e9}

  Execution
    the first failing expectation stops an Example execution                                         ${g2().e1}
    the first error stops an Example execution                                                       ${g2().e2}
    the first skipped expectation skips the Example execution                                        ${g2().e3}
    the failure method throws a FailureException                                                     ${g2().e4}
                                                                                                     """

  "fragments" - new g1 with HasAMutableSpec {
    e1 := { contentString must contain("it should") }
    e2 := { contentList must contain(allOf("Text(it should)", "Example(have one example)", "Example(have failing example)")).inOrder }
    e3 := { contentList must contain(allOf("Step", "Text(it should)")).inOrder }
    e4 := { fragments.toList must beLike { case SpecStart(_,a,_,_) :: other => a.xonly must beTrue } }
    e5 := { contentList must contain(allOf("Text(examples can)", "Text(be nested)", "Example(at level 1)", "Example(at level 2)")).inOrder }
    e6 := { contentString must contain("should expectation") }
    e7 := { contentString must contain("should also") }
    e8 := { contentList must contain(allOf("Example(example 1)", "Example(example 2)")) }
    e9 := { contentList must contain("Example(several expectations)") }
  }

  "execution" - new g2 with FragmentExecution with HasAMutableSpec {
    e1 := {
      fragments.map(executeFragment(args())(_))
      output.messages must not contain("statement executed after failing expectation")
    }
    e2 := {
      fragments.map(executeFragment(args())(_))
      output.messages must not contain("statement executed after error expectation")
    }
    e3 := {
      fragments.map(executeFragment(args())(_))
      output.messages must not contain("statement executed after skipped expectation")
    }
    e4 := { new Specification { failure("failed") } must throwA[FailureException] }
  }

  trait HasAMutableSpec {
    val output = new StringOutput {}
    def spec = new Specification {
      xonly
      step(output.println("hello"))
      "it" should {
        "have one example" in { 1 must_== 1 }
        "have failing example" in {
          1 must_== 2
          output.println("statement executed after failing expectation")
          success
        }
        "have an error example" in {
          {throw new Error("error"); 1} must_== 1
          output.println("statement executed after error expectation")
          success
        }
        "have a skipped example" in {
          1 must be_==(2).orSkip
          output.println("statement executed after skipped expectation")
          success
        }
        "have an example using a should expectation" in {
          1 should be_==(1)
        }
      }
      "examples" can {
        "be nested" >> {
          "at level 1" in { success }
          "at level 2" in { success }
        }
      }

      "it should also"^
        "have another example" ! success

      "a block of examples" >> {
        examplesBlock(Seq(1, 2).foreach(i => "example "+i >> success))
      }
      "several expectations" in {
        Result.unit(Seq(1, 2, 3).foreach(i => i must be_>(0)))
      }
    }
    def fragments = spec.content.fragments
    def contentList = fragments.map(_.toString).toList
    def contentString = contentList.mkString("\n")
  }

}
