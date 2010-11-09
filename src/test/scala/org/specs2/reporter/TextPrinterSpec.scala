package org.specs2
package reporter
import io.MockOutput
import main.Arguments
import specification._

class TextPrinterSpec extends SpecificationWithJUnit { def is = literate                  ^
                                                                                          """
  The TextPrinter is folding Executed Fragments and exporting them
  to a ResultOutput trait knowing how to output successes, failures,...
                                                                                          """^end^
"  Text presentation"                                                                     ^
"    by default the Text and the examples are properly indented"                          ! prez().e1
"    if noindent = true then there is no automatic indenting"                             ^
"    if xonly = true"                                                                     ^
"      text is not shown"                                                                 ^
"      successful examples are not shown"                                                 ^
"      skipped examples are not shown"                                                    ^
"      pending examples are not shown"                                                    ^
"      failure examples are shown"                                                        ^
"      error examples are shown"                                                          ^
"      statistics are not shown if the spec is a success"                                 ^
"      statistics are shown if there is a failure or an error"                            ^
"    if failtrace = true, failures stacktraces are shown"                                 ^
                                                                                          p^
"  Statuses"                                                                              ^
"    regular text must have no status"                                                    ^
"    a successful example must be displayed with a +"                                     ^
"    a failed example must be displayed with a x"                                         ^
"    an error example must be displayed with a !"                                         ^
"    a skipped example must be displayed with a o"                                        ^
"    a pending example must be displayed with a o"                                        ^
                                                                                          p^
"  Statistics"                                                                            ^
"    must show the number of examples"                                                    ^
"    must show the number of expectations"                                                ^
"      not if they are the same as the number of examples"                                ^
"      if they are not the same as the number of examples"                                ^
"    must show the number of failures"                                                    ^
"    must show the number of errors"                                                      ^
"    must show the execution time"                                                        ^
                                                                                          end

  implicit val args = Arguments()                                                                                          
  case class prez() {
    def e1 = print("t1" ^ "e1"!success ^ "e2"! success) must 
             contain("  t1",
                     "  + e1",
                     "  + e2")
  }
  def print(fragments: Fragments)(implicit args: Arguments): Seq[String] = {
    val executed = new DefaultExecutionStrategy() {}.execute(args)(List(fragments.fragments))
    val exporter = new TextExporting with MockOutput {}
    exporter.export(args)(executed)
    exporter.messages
  }
}