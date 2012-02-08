package org.specs2
package reporter

import specification._
import main._
import io._
import org.scalacheck._

class SelectionSpec extends Specification with ScalaCheck with ArbitraryFragments { def is =
                                                                                                                        """
Before executing and reporting a specification, the fragments must be selected and sorted:

 * they must be selected to keep only the relevant ones
 * they must be sorted to respect their execution dependencies
   ** steps must be executed before examples as specified
   ** the 'sequential' argument forces the each fragment to be sequenced

                                                                                                                        """^p^
  "First of all examples are filtered"                                                                                  ^
    "when the user specifies a regular expression: ex = ex1.*"                                                          ^
      "in the spec"                                                                                                     ! filter().e1^
      "on the command line"                                                                                             ! filter().e2^
    "if no filter is specified, nothing must be filtered out"                                                           ! filter().e3^
                                                                                                                        p^
                                                                                                                        """
  Then the Selection trait groups fragments to execute in lists of Fragments which can
  be executed concurrently.                                                                                             """^
                                                                                                                        p^
  "If a specification contains steps they must be grouped before the examples"                                          ^
    "2 consecutive steps must not be in the same list"                                                                  ! steps().e1^
    "2 consecutive examples must be in the same list"                                                                   ! steps().e2^
    "an example followed by a step must not be in the same list"                                                        ! steps().e3^
    "a step followed by an example must not be in the same list"                                                        ! steps().e4^
    "so that steps and examples are always separate"                                                                    ! steps().e5^
                                                                                                                        p^
  "If a specification contains the 'sequential' argument"                                                               ^
    "all examples must be executed in a sequence"                                                                       ! seq().e1^
    "with a Reporter"                                                                                                   ! seq().e2^
                                                                                                                        p^
  "It is possible to select only some previously executed fragments"                                                    ^
    "wasIssue selects only the fragments which were failed or in error"                                                 ! rerun().e1^
                                                                                                                        end
  
  case class filter() extends WithSelection {
    def e1 = select(args(ex = "ex1") ^ ex1 ^ ex2).toString must not contain("ex2")
    def e2 = select(ex1 ^ ex2)(Arguments("ex", "ex1")).toString must not contain("ex2")
    def e3 = select(ex1 ^ ex2).toString must contain("ex1")
  }

  case class steps() extends ScalaCheck with WithSelection {
    implicit val params = set(maxSize -> 3)

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
      selectSequence(fragments).map((s: FragmentSeq) => s.fragments.toString) must contain(
      "List(SpecStart(), Text(intro), Step)",
      "List(Example(ex1), Example(ex2))",
      "List(Step)",
      "List(Step)",
      "List(Example(ex1), Example(ex2), SpecEnd())").inOrder
    }
  }

  case class seq() extends WithSelection {
    def e1 = {
      val fragments: Fragments = sequential ^ example("e1") ^ step("s1") ^ example("e2")
      select(fragments).toString must_== "List(SpecStart(Object), Example(e1), Step, Example(e2), SpecEnd(Object))"
    }
    def e2 = {
      val spec = new Specification { def is = sequential ^ example("e1") ^ step("s1") ^ example("e2") }
      reporter.report(spec)(main.Arguments())
      reporter.messages must contain("e1", "s1", "e2").inOrder
    }
  }

  case class rerun() extends WithSelection {
    /**
     * The storing trait 'decides' to keep only the example 1 because of a previous run
     */
    override val selection = new DefaultSelection with DefaultSequence with DefaultStoring with MockOutput {
      override def includePrevious(specName: SpecName, e: Example, args: Arguments) = e.desc.toString == "e1"
    }

    def e1 = {
      val fragments: Fragments = wasIssue ^ sequential ^ example("e1") ^ step("s1") ^ example("e2")
      select(fragments).toString must_== "List(SpecStart(Object), Example(e1), Step, SpecEnd(Object))"
    }
  }

  val ex1 = "ex1" ! success
  val ex2 = "ex2" ! success

  trait WithSelection {
    val selection = new DefaultSelection with DefaultSequence with MockOutput

    def selectSequence(fs: Fragments): Seq[FragmentSeq] = {
      selection.sequence(selection.select(fs.arguments)(SpecificationStructure(fs)).fragments.fragments)(fs.arguments).toList
    }
    def select(f: Fragments)(implicit args: Arguments = Arguments()) = {
      val fs = new Specification { def is = f }.content
      selection.select(args)(SpecificationStructure(fs)).content.fragments.toList.map(_.toString)
    }
    def step(message: String) = Step({selection.println(message); reporter.println(message)})
    def example(message: String) = message ! { selection.println(message); reporter.println(message); success }
    val reporter = new DefaultReporter with Exporting with MockOutput {
      def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => spec.executed
    }
  }

}
