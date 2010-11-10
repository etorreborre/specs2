package org.specs2
package reporter
import io.MockOutput
import main.Arguments
import execute._
import specification._

class TextPrinterSpec extends SpecificationWithJUnit { def is = args(xonly=false)         ^
                                                                                          """
  The TextPrinter is folding Executed Fragments and exporting them
  to a ResultOutput trait knowing how to output successes, failures,...
                                                                                          """^endp^
"  Text presentation"                                                                     ^
"    by default the Text and the examples are properly indented"                          ! prez().e1^
"    if noindent = true then there is no automatic indenting"                             ! prez().e2^
"    if xonly = true"                                                                     ^t^ 
"      text is not shown"                                                                 ! xonly().e1^
"      successful examples are not shown"                                                 ! xonly().e2^
"      skipped examples are not shown"                                                    ! xonly().e3^
"      pending examples are not shown"                                                    ! xonly().e4^
"      failure examples are shown"                                                        ! xonly().e5^
"      error examples are shown"                                                          ! xonly().e6^
"      statistics shown"                                                                  ! xonly().e7^bt^
"    if failtrace = true, failures stacktraces are shown"                                 ! failtrace().e1^
"    if plan = true, nothing is executed"                                                 ! plan().e1^
"    if color = true, the text is colorized"                                              ^
"      text is blue"                                                                      ! color().e1^
"      success is green"                                                                  ! color().e2^
"      failures are red"                                                                  ! color().e3^
"      errors are red"                                                                    ! color().e4^
"      others are white"                                                                  ! color().e5^
                                                                                          p^
"  Statuses"                                                                              ^
"    regular text must have no status"                                                    ! status().e1^
"    a successful example must be displayed with a +"                                     ! status().e2^
"    a failed example must be displayed with a x"                                         ! status().e3^
"    an error example must be displayed with a !"                                         ! status().e4^
"    a skipped example must be displayed with a o"                                        ! status().e5^
"    a pending example must be displayed with a *"                                        ! status().e6^
                                                                                          p^
"  Statistics must show"                                                                  ^
"    the number of examples"                                                              ! stats().e1^
"    the number of expectations"                                                          ^t^
"      not if they are the same as the number of examples"                                ! stats().e2^
"      if they are not the same as the number of examples"                                ! stats().e3^bt^
"    the number of failures"                                                              ! stats().e4^
"    the number of errors"                                                                ! stats().e5^
"    the execution time"                                                                  ! stats().e6^
                                                                                          end

  implicit val default = Arguments()
  val t1       = "t1"
  val ex1      = "e1" ! success
  val ex2      = "e2" ! success
  val fail3    = "fail3" ! failure
  val error4   = "error4" ! anError
  val skipped5 = "skip it" ! skipped
  val pending6 = "todo" ! pending
  
  case class prez() {
    val noindent: Arguments = args(noindent = true)
    
    def e1 = print(t1 ^ ex1 ^ ex2) must 
             contain("  t1",
                     "  + e1",
                     "  + e2")
    
    def e2 = print(noindent ^ t1 ^ "  e1"!success ^ " e2"! success) must 
             contain("t1",
                     "  + e1",
                     " + e2")
  }
  case class xonly() {
    val xonly: Arguments = args(xonly = true)
    
    def e1 = print(xonly ^ t1 ^ ex1 ^ fail3) must not containMatch("t1")
    def e2 = print(xonly ^ t1 ^ ex1 ^ fail3) must not containMatch("e1")
    def e3 = print(xonly ^ t1 ^ skipped5 ^ fail3) must not containMatch("skip it")
    def e4 = print(xonly ^ t1 ^ pending6 ^ fail3) must not containMatch("todo")
    def e5 = print(xonly ^ t1 ^ ex1 ^ fail3) must containMatch("fail3")
    def e6 = print(xonly ^ t1 ^ ex1 ^ error4) must containMatch("error4")
    def e7 = print(xonly ^ t1 ^ ex1 ^ ex2) must containMatch("examples")
  }
  case class color() {
    val color: Arguments = args(color = true)
    import text.AnsiColors._
    import text.Trim._
    
    def e1 = print(color ^ t1) must containMatch(blue.remove("\033["))
    def e2 = print(color ^ ex1) must containMatch(green.remove("\033["))
    def e3 = print(color ^ fail3) must containMatch(red.remove("\033["))
    def e4 = print(color ^ error4) must containMatch(red.remove("\033["))
    def e5 = print(color ^ pending6) must containMatch(white.remove("\033["))
  }
  case class failtrace() {
    val failtrace: Arguments = args(failtrace = true)
    def e1 = print(failtrace ^ t1 ^ ex1 ^ fail3) must containMatch("org.specs2")
  }
  case class plan() {
    val plan: Arguments = args(plan = true)
    def e1 = print(plan ^ t1 ^ ex1 ^ fail3) must contain("  e1") and not containMatch("\\+ e1") 
  }
  case class status() {
    def e1 = print(t1 ^ ex1) must containMatch("^\\s*t1") 
    def e2 = print(t1 ^ ex1) must contain("  + e1") 
    def e3 = print(t1 ^ fail3) must contain("  x fail3") 
    def e4 = print(t1 ^ error4) must contain("  ! error4") 
    def e5 = print(t1 ^ skipped5) must contain("  o skip it") 
    def e6 = print(t1 ^ pending6) must containMatch("  \\* todo") 
  }
  case class stats() {
    def e1 = print(t1 ^ ex1) must containMatch("1 example") 
    def e2 = print(t1 ^ ex1) must not containMatch("expectation") 
    def e3 = print(t1 ^ "ex1"!Success("ok", 2)) must containMatch("2 expectations") 
    def e4 = print(t1 ^ fail3) must containMatch("1 failure") 
    def e5 = print(t1 ^ error4) must containMatch("1 error") 
    def e6 = print(t1 ^ ex1) must containMatch("0 second") 
  }

  def print(fragments: Fragments): Seq[String] = {
    val execution = new DefaultExecutionStrategy() {}
    val exporter = new TextExporting with MockOutput {}
    val executed = execution.execute(fragments.arguments)(
                     List(SpecStart("spec") +: fragments.fragments :+ SpecEnd("spec")))
    exporter.export(fragments.arguments)(executed)
    exporter.messages
  }
}
