package org.specs2
package mutable
import io._
import specification.{ SpecStart, FragmentExecution }

class MutableSpecificationSpec extends org.specs2.SpecificationWithJUnit { def is =
                                                                                                                     """
To ease the migration for specs users, from specs to specs2, it is possible to write specifications which look
almost like specs specifications, with `should` blocks and `in` examples.

The following examples specify the functionalities for such a mutable specification.
                                                                                                                    """^
  "Fragments creation"                                                                                              ^
    "a `should` block creates a text fragment"                                                                      ! fragments().e1^
    "every example in a should block creates examples fragments following the `should` text"                        ! fragments().e2^
    "an action can be created with the `action` keyword"                                                            ! fragments().e3^
    "arguments can be created with the `args` keyword"                                                              ! fragments().e4^
                                                                                                                    p^
  "Execution"                                                                                                       ^
    "the first failing expectation stops an Example execution"                                                      ! execution().e1^
                                                                                                                    end


  case class fragments() extends HasAMutableSpec {

    def e1 = contentString must contain("it should")
    def e2 = contentList must contain("Text(it should)", "Example(have one example)", "Example(have failing example)").inOrder
    def e3 = contentList must contain("Step", "Text(it should)").inOrder
    def e4 = fragments.toList must beLike { case SpecStart(_, a) :: other => a.xonly must beTrue }
  }

  case class execution() extends FragmentExecution with HasAMutableSpec {
    def e1 = {
      fragments.map(executeFragment(args())(_))
      output.messages must not contain("statement executed after failing expectation")
    }
  }

  trait HasAMutableSpec {
    val output = new MockOutput {}
    def spec = new Specification {
      xonly
      step(output.println("hello"))
      "it" should {
        "have one example" in { 1 must_== 1 }
        "have failing example" in {
          1 must_== 2
          output.println("statement executed after failing expectation")
          1 must_== 1
        }
      }
    }
    def fragments = spec.content.fragments
    def contentList = fragments.map(_.toString)
    def contentString = contentList.mkString("\n")
  }

}