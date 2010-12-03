package org.specs2
package reporter
import scalaz.Scalaz
import Scalaz._
import text.AnsiColors._
import io.MockOutput
import main.Arguments
import execute._
import specification._
import SpecsArguments._

class TextPrinterSpec extends SpecificationWithJUnit { def is = 
                                                                                          """
The TextPrinter is folding Executed Fragments and exporting them
to a ResultOutput trait knowing how to output successes, failures,...
                                                                                          """^
																						  p^
"Text presentation"                                                                       ^
  "by default the Text and the examples are properly indented"                            ! prez().e1^
  "if noindent = true then there is no automatic indenting"                               ! prez().e2^
  "if xonly = true"                                                                       ^ 
    "text is not shown"                                                                   ! xonlyargs().e1^
    "successful examples are not shown"                                                   ! xonlyargs().e2^
    "skipped examples are not shown"                                                      ! xonlyargs().e3^
    "pending examples are not shown"                                                      ! xonlyargs().e4^
    "failure examples are shown"                                                          ! xonlyargs().e5^
    "error examples are shown"                                                            ! xonlyargs().e6^
    "statistics shown"                                                                    ! xonlyargs().e7^
  "if failtrace = true, failures stacktraces are shown"                                   ! failtrace().e1^bt^
  "if plan = true, nothing is executed"                                                   ! plan().e1^
  "if sequential = false examples are executed concurrently"                              ! seq().e1^
  "if sequential = true examples are executed sequentially"                               ! seq().e2^
  "if color = true, the text output is colorized"                                         ^
    "text is white"                                                                       ! color().e1^
    "success status is green"                                                             ! color().e2^
    "failures status is yellow"                                                           ! color().e3^
    "errors status are red"                                                               ! color().e4^
    "pending status is blue"                                                              ! color().e5^
    "skipped status is cyan"                                                              ! color().e6^
    "stats are blue"                                                                      ! color().e7^
                                                                                          endp^
"Statuses"                                                                                ^
  "regular text must have no status"                                                      ! status().e1^
  "a successful example must be displayed with a +"                                       ! status().e2^
  "a failed example must be displayed with a x"                                           ! status().e3^
  "an error example must be displayed with a !"                                           ! status().e4^
  "a skipped example must be displayed with a o"                                          ! status().e5^
  "a pending example must be displayed with a *"                                          ! status().e6^
  "a multi-line description must be indented ok"                                          ! status().e7^
                                                                                          p^
"Statistics must show"                                                                    ^
  "the number of examples"                                                                ! stats().e1^
  "the number of expectations"                                                            ^
    "not if they are the same as the number of examples"                                  ! stats().e2^
    "if they are not the same as the number of examples"                                  ! stats().e3^bt^
  "the number of failures"                                                                ! stats().e4^
  "the number of errors"                                                                  ! stats().e5^
  "the execution time"                                                                    ! stats().e6^
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
    val noindent = args(noindent = true)
    
    def e1 = print(t1 ^ ex1 ^ ex2) must 
             contain("t1",
                     "+ e1",
                     "+ e2")
    
    def e2 = print(noindent ^ t1 ^ "  e1"!success ^ " e2"! success) must 
             contain("t1",
                     "+ e1",
                     "+ e2")
  }
  case class xonlyargs() {
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
    import text.AnsiColors._
    import text.Trim._
    
    def e1 = printWithColors(t1) must containMatch(white.remove("\033["))
    def e2 = printWithColors(ex1) must containMatch(green.remove("\033["))
    def e3 = printWithColors(fail3) must containMatch(yellow.remove("\033["))
    def e4 = printWithColors(error4) must containMatch(red.remove("\033["))
    def e5 = printWithColors(pending6) must containMatch(blue.remove("\033["))
    def e6 = printWithColors(skipped5) must containMatch(cyan.remove("\033["))
    def e7 = printWithColors(t1) must containMatch(blue.remove("\033["))
  }
  case class failtrace() {
    val failtrace: Arguments = args(failtrace = true)
    def e1 = print(failtrace ^ t1 ^ ex1 ^ fail3) must containMatch("org.specs2")
  }
  case class plan() {
    val plan: Arguments = args(plan = true)
    def e1 = print(plan ^ t1 ^ ex1 ^ fail3) must contain("  e1") and not containMatch("\\+ e1") 
  }
  case class seq() {
    val sequential: Arguments = args(sequential = true)
    val messages = new MockOutput {}
    val slowex1 = "e1" ! { Thread.sleep(20); messages.println("e1"); success }
    val fastex2 = "e2" ! { messages.println("e2"); success }
    def e1 = {
      print(args(noindent = true, color = false) ^ slowex1 ^ fastex2) 
      messages.messages must containInOrder("e2", "e1")
    }
    def e2 = {
      print(args(sequential = true, noindent = true, color = false) ^ slowex1 ^ fastex2)
      messages.messages must containInOrder("e1", "e2")
    }
  }
  case class status() {
    def e1 = print(t1 ^ ex1) must containMatch("^\\s*t1") 
    def e2 = print(t1 ^ ex1) must contain("+ e1") 
    def e3 = print(t1 ^ fail3) must contain("x fail3") 
    def e4 = print(t1 ^ error4) must contain("! error4") 
    def e5 = print(t1 ^ skipped5) must contain("o skip it") 
    def e6 = print(t1 ^ pending6) must containMatch("\\* todo") 
    def e7 = print(t1 ^ "e1\nexample1" ! success) must contain(
        "+ e1",
        "  example1") 
  }
  case class stats() {
    def e1 = print(t1 ^ ex1) must containMatch("1 example") 
    def e2 = print(t1 ^ ex1) must not containMatch("expectation") 
    def e3 = print(t1 ^ "ex1"!Success("ok", 2)) must containMatch("2 expectations") 
    def e4 = print(t1 ^ fail3) must containMatch("1 failure") 
    def e5 = print(t1 ^ error4) must containMatch("1 error") 
    def e6 = print(t1 ^ ex1) must containMatch("0 second") 
  }

  def printWithColors(fragments: Fragments): Seq[String] = {
    val selection = new DefaultSelection() {}
    val execution = new DefaultExecutionStrategy() {}
    val selected = selection.select(fragments.arguments)(Fragments(SpecStart("spec") +: fragments.fragments :+ SpecEnd("spec"))(fragments.arguments))
    val executed = execution.execute(fragments.arguments)(selected)
    val mockOutput = new TextResultOutput with MockOutput
    val printer = new TextPrinter {
      override val output = mockOutput
    }
    printer.print(getClass, executed)(fragments.arguments)
    mockOutput.messages
  }
  def print(fragments: Fragments): Seq[String] = printWithColors(fragments).map(removeColors(_))
}
