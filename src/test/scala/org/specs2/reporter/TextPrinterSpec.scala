package org.specs2
package reporter
import io.MockOutput
import main.Arguments
import specification._

class TextPrinterSpec extends SpecificationWithJUnit { def is = //literate                  ^
                                                                                          """
  The TextPrinter is folding Executed Fragments and exporting them
  to a ResultOutput trait knowing how to output successes, failures,...
                                                                                          """^end^
"  Text presentation"                                                                     ^
"    by default the Text and the examples are properly indented"                          ! prez().e1^
"    if noindent = true then there is no automatic indenting"                             ! prez().e2^
"    if xonly = true"                                                                     ^ 
"      text is not shown"                                                                 ! xonly().e1^
"      successful examples are not shown"                                                 ! pending^
"      skipped examples are not shown"                                                    ! pending^
"      pending examples are not shown"                                                    ! pending^
"      failure examples are shown"                                                        ! pending^
"      error examples are shown"                                                          ! pending^
"      statistics are not shown if the spec is a success"                                 ! pending^
"      statistics are shown if there is a failure or an error"                            ! pending^
"    if failtrace = true, failures stacktraces are shown"                                 ! pending^
                                                                                          p^
"  Statuses"                                                                              ^
"    regular text must have no status"                                                    ! pending^
"    a successful example must be displayed with a +"                                     ! pending^
"    a failed example must be displayed with a x"                                         ! pending^
"    an error example must be displayed with a !"                                         ! pending^
"    a skipped example must be displayed with a o"                                        ! pending^
"    a pending example must be displayed with a o"                                        ! pending^
                                                                                          p^
"  Statistics must show"                                                                  ^
"    the number of examples"                                                              ! pending^t^
"    the number of expectations"                                                          ^
"      not if they are the same as the number of examples"                                ! pending^
"      if they are not the same as the number of examples"                                ! pending^u^
"    the number of failures"                                                              ! pending^
"    the number of errors"                                                                ! pending^
"    the execution time"                                                                  ! pending^
                                                                                          end

  implicit val default = Arguments()
 
  case class prez() {
    val noindent: Arguments = args(noindent = true)
    
    def e1 = print("t1" ^ "e1"!success ^ "e2"! success) must 
             contain("  t1",
                     "  + e1",
                     "  + e2")
    
    def e2 = print(noindent ^ "t1" ^ "  e1"!success ^ " e2"! success) must 
             contain("t1",
                     "  + e1",
                     " + e2")
  }
  case class xonly() {
    val xonly: Arguments = args(xonly = true)
    def e1 = print(xonly ^ "t1" ^ "e1"!success ^ "e2"! failure) must not contain("  t1")
    
  }

  def print(fragments: Fragments): Seq[String] = {
    val execution = new DefaultExecutionStrategy() {}
    val exporter = new TextExporting with MockOutput {}
    val executed = execution.execute(fragments.arguments)(
                     List(SpecStart("spec") +: fragments.fragments))
    exporter.export(fragments.arguments)(executed)
    exporter.messages
  }
}
