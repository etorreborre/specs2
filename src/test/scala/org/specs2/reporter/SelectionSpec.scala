package org.specs2
package reporter

import specification._
import main._
import io._
import org.scalacheck._
import execute.Success

class SelectionSpec extends Specification with Tags { def is =
                                                                                                                        """
 Before executing and reporting a specification, the fragments must be selected:

 * with the ex argument
 * with tags                                                                                                            """^
                                                                                                                        p ^
 "First of all examples are filtered"                                                                                   ^
   "when the user specifies a regular expression: ex = ex1.*"                                                           ^
     "in the spec"                                                                                                      ! filter().e1^
     "on the command line"                                                                                              ! filter().e2^
     "if the regexp is invalid, it is quoted"                                                                           ! filter().e3^tag("x")^
   "if no filter is specified, nothing must be filtered out"                                                            ! filter().e4^
                                                                                                                        p^
 "It is possible to select only some previously executed fragments"                                                     ^
   "wasIssue selects only the fragments which were failed or in error"                                                  ! rerun().e1^
                                                                                                                        p^
 "if a specification contains the 'isolated' argument"                                                                  ^
   "examples bodies must be copied"                                                                                     ! isolate().e1^
   "along with all the previous steps"                                                                                  ! isolate().e2^
   "steps bodies must not be copied"                                                                                    ! isolate().e3^
   "actions bodies must be copied"                                                                                      ! isolate().e4^
   "if the examples, steps or actions are marked as global, they are never copied"                                      ! isolate().e5^t^
     "with a global step before an example"                                                                             ! isolate().e6^
                                                                                                                        end
  
  case class filter() extends WithSelection {
    def e1 = select(args(ex = "ex1") ^ ex1 ^ ex2).toString must not contain("ex2")
    def e2 = select(ex1 ^ ex2)(Arguments("ex", "ex1")).toString must not contain("ex2")
    def e3 = select(ex3 ^ ex2)(Arguments("ex", "(+40k)")).toString must contain("(+40k)")
    def e4 = select(ex1 ^ ex2).toString must contain("ex1")
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

  case class isolate() extends WithSelection {
    implicit val arguments = main.Arguments()

    def isIsolated(spec: SpecificationWithLocalVariable, expectedLocalValue: Int) = {
      reporter.report(spec)
      spec.i === expectedLocalValue
    }

    def e1 = isIsolated(new SpecificationWithLocalVariable { def is = isolated ^ "e1" ! { i = 1; ok } }, expectedLocalValue = 0)
    def e2 = isIsolated(new SpecificationWithLocalVariable { def is = isolated ^ Step(i = 1) ^ "e1" ! ok ^ Step(i = 2) }, expectedLocalValue = 2)
    def e3 = isIsolated(new SpecificationWithLocalVariable { def is = isolated ^ Step(i = 1) }, expectedLocalValue = 1)
    def e4 = isIsolated(new SpecificationWithLocalVariable { def is = isolated ^ Action(i = 1) }, expectedLocalValue = 0)

    def e5 = isIsolated(new SpecificationWithLocalVariable { def is = isolated ^ ("e1" ! { i = 1; ok }).global }, expectedLocalValue = 1)
    def e6 = isIsolated(new SpecificationWithLocalVariable { def is = isolated ^ Step(i = 1).global ^ "e1" ! { i = 2; ok } }, expectedLocalValue = 1)
  }

  trait SpecificationWithLocalVariable extends Specification {
    var i = 0
  }

  val ex1 = "ex1" ! success
  val ex2 = "ex2" ! success
  val ex3 = "(+40k)" ! success

}

trait WithSelection extends FragmentsBuilder {
  val selection = new DefaultSelection with DefaultSequence with MockOutput

  def selectSequence(fs: Fragments): Seq[FragmentSeq] = {
    selection.sequence(fs.specName, selection.select(fs.arguments)(SpecificationStructure(fs)).is.fragments)(fs.arguments).toList
  }
  def select(f: Fragments)(implicit args: Arguments = Arguments()) = {
    val fs = new Specification { def is = f }.content
    selection.select(args)(SpecificationStructure(fs)).content.fragments.toList.map(_.toString)
  }
  def step(message: String) = Step({selection.println(message); reporter.println(message)})
  def example(message: String) = message ! { selection.println(message); reporter.println(message); Success() }
  val reporter = new DefaultReporter with Exporting with MockOutput {
    def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => spec.executed
  }
}

