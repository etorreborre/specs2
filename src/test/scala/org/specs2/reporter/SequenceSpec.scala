package org.specs2
package reporter

import specification._

class SequenceSpec extends Specification with ScalaCheck with ArbitraryFragments { def is = s2"""

 Before executing and reporting a specification, the fragments must be arranged for execution:

  * steps must be executed before examples as specified
  * the 'sequential' argument forces the each fragment to be sequenced
  * if the 'isolated' argument is present then Examples/Steps/Actions bodies must be copied
    so that they'll be executed in a separate Specification instance
                                                                                                                       
                                                                                                                       
  if a specification contains steps they must be grouped before the examples                                         
    2 consecutive steps must not be in the same list                                              ${steps().e1}
    2 consecutive examples must be in the same list                                               ${steps().e2}
    an example followed by a step must not be in the same list                                    ${steps().e3}
    a step followed by an example must not be in the same list                                    ${steps().e4}
    in any specification steps and examples are always separated                                  ${steps().e5}

  if a specification contains the 'sequential' argument
    all examples must be executed in a sequence                                                   ${seq().e1}
    with a Reporter                                                                               ${seq().e2}
    included specifications must have their own arguments                                         ${seq().e3}
                                                                                                                        """

  case class steps() extends ScalaCheck with WithSelection {
    implicit val params = set(maxSize = 3)

    def e1 = check { (fs: Fragments) =>
      val selected = selectSequence(fs ^ step("1"))
      val selected2 = selectSequence(fs ^ step("1") ^ step("2"))
      selected2 must not have size(selected.size)
    }
    def e2 = check { (fs: Fragments) =>
      val selected = selectSequence(fs ^ ex1)
      val selected2 = selectSequence(fs ^ ex1 ^ ex2)
      selected2 must have size(selected.size)
    }
    def e3 = check { (fs: Fragments) =>
      val selected = selectSequence(fs ^ ex1)
      val selected2 = selectSequence(fs ^ ex1 ^ step("1"))
      selected2 must have size(selected.size + 1)
    }
    def e4 = check { (fs: Fragments) =>
      val selected = selectSequence(fs ^ step("1"))
      val selected2 = selectSequence(fs ^ step("1") ^ ex2)
      selected2 must have size(selected.size + 1)
    }
    def e5 = {
      val fragments: Fragments = "intro" ^ step("1") ^ ex1 ^ ex2 ^ step("2") ^ step("3") ^ ex1 ^ ex2
      selectSequence(fragments).map((s: FragmentSeq) => s.fragments.toString) must contain(allOf(
        "List(SpecStart(), Text(intro), Step)",
        "List(Example(ex1), Example(ex2))",
        "List(Step)",
        "List(Step)",
        "List(Example(ex1), Example(ex2), SpecEnd())")).inOrder
    }
  }

  case class seq() extends WithSelection {
    def e1 = {
      val fragments: Fragments = sequential ^ example("e1") ^ step("s1") ^ example("e2")
      select(fragments).toString must_== "List(SpecStart(Specification), Example(e1), Step, Example(e2), SpecEnd(Specification))"
    }
    def e2 = {
      val spec = new Specification { def is = sequential ^ example("e1") ^ step("s1") ^ example("e2") }
      reporter.report(spec)(main.Arguments())
      reporter.messages must contain(allOf("e1", "s1", "e2")).inOrder
    }
    def e3 = {
      val inner = new Specification { def is = sequential ^ "i1" ! ok ^ "i2" ! ok}
      val fragments: Fragments = "intro" ^ step("1") ^ ex1 ^ ex2 ^ step("2") ^ inner ^ step("3") ^ ex1 ^ ex2
      selectSequence(fragments).map((s: FragmentSeq) => (s.fragments, s.arguments.sequential).toString) must contain(allOf(
        "(List(SpecStart(), Text(intro), Step),false)",
        "(List(Example(ex1), Example(ex2)),false)",
        "(List(Step),false)",
        "(List(SpecStart(Specification), Example(i1), Example(i2), SpecEnd(Specification)),true)",
        "(List(Step),false)",
        "(List(Example(ex1), Example(ex2), SpecEnd()),false)")).inOrder

    }
  }

  val ex1 = "ex1" ! success
  val ex2 = "ex2" ! success

}
