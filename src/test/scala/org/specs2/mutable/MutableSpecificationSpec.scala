package org.specs2
package mutable
import io._
import specification.{ SpecStart, FragmentExecution }
import execute.FailureException

class MutableSpecificationSpec extends org.specs2.Specification { def is =
                                                                                                                        """
A Specification can be written in the specs style using should/in blocks. This works by building the fragments
and mutating a local variable

The following examples specify the functionalities for such a mutable specification.
                                                                                                                        """^
  "Fragments creation"                                                                                                  ^
    "a `should` block creates a text fragment"                                                                          ! fragments().e1^
    "every example in a should block creates examples fragments following the `should` text"                            ! fragments().e2^
    "an action can be created with the `action` keyword"                                                                ! fragments().e3^
    "arguments can be created with the `args` keyword"                                                                  ! fragments().e4^
    "examples can be nested"                                                                                            ! fragments().e5^
    "should expectations can be used"                                                                                   ! fragments().e6^
    "a text fragment can start a block with ^"                                                                          ! fragments().e7^
    "foreach can be used to create a block of examples"                                                                 ! fragments().e8^
                                                                                                                        p^
  "Execution"                                                                                                           ^
    "the first failing expectation stops an Example execution"                                                          ! execution().e1^
    "the first error stops an Example execution"                                                                        ! execution().e1_1^
    "the first skipped expectation skips the Example execution"                                                         ! execution().e2^
    "the failure method throws a FailureException"                                                                      ! execution().e3^
                                                                                                                        end




  case class fragments() extends HasAMutableSpec {

    def e1 = contentString must contain("it should")
    def e2 = contentList must contain("Text(it should)", "Example(have one example)", "Example(have failing example)").inOrder
    def e3 = contentList must contain("Step", "Text(it should)").inOrder
    def e4 = fragments.toList must beLike { case SpecStart(_,a,_) :: other => a.xonly must beTrue }
    def e5 = contentList must contain("Text(examples can)", "Text(be nested)", "Example(at level 1)", "Example(at level 2)").inOrder
    def e6 = contentString must contain("should expectation")
    def e7 = contentString must contain("should also")
    def e8 = contentList must contain("Example(example 1)", "Example(example 2)")
  }

  case class execution() extends FragmentExecution with HasAMutableSpec {
    def e1 = {
      fragments.map(executeFragment(args())(_))
      output.messages must not contain("statement executed after failing expectation")
    }
    def e1_1 = {
      fragments.map(executeFragment(args())(_))
      output.messages must not contain("statement executed after error expectation")
    }
    def e2 = {
      fragments.map(executeFragment(args())(_))
      output.messages must not contain("statement executed after skipped expectation")
    }
    def e3 = new Specification { failure("failed") } must throwA[FailureException]
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
        Seq(1, 2) foreach { i =>
          "example "+i >> success
        }
      }
    }
    def fragments = spec.content.fragments
    def contentList = fragments.map(_.toString)
    def contentString = contentList.mkString("\n")
  }

}