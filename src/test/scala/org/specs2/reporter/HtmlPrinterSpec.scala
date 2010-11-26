package org.specs2
package reporter
import io._
import specification._

class HtmlPrinterSpec extends SpecificationWithJUnit { def is = 
                                                                                          """
  The HtmlPrinter class is responsible for opening an html file to write
  the specification text.
                                                                                          """^
                                                                                          p^
  "The file path must"                                                                    ^
    "use target/specs-reports as a default value for the output directory"                ! filepath().e1^
    "use the outDir system variable if set"                                               ! filepath().e2^
    "use class name of the specification as file name"                                    ! filepath().e3^
                                                                                          p^
  "The page title"                                                                        ^
    "must be the title of the specification"                                              ! title().e1^
                                                                                          end
                                                                                          
  implicit val argument = args()
  case class filepath() {
    def e1 = printer.reportPath(getClass) must startWith("target/specs2-reports")
    def e2 = {
      val printer = new HtmlPrinter {
        override lazy val outputDir = "output/"
      }
      printer.reportPath(getClass) must startWith("output/")
    }
    def e3 =  printer.reportPath(getClass) must endWith(getClass.getName + ".html")
  }

  case class title() extends MockHtmlPrinter {
    val spec: Fragments = "Specification".title ^ "t1"
    def e1 = print(spec) must contain("<title>Specification</title>")
  }
  
  trait MockHtmlPrinter extends FragmentExecution {
    def printer = new HtmlPrinter with MockFileWriter {}
    val out = new MockWriter {}
    
    def print(spec: Fragments) = {
      printer.printLines(spec.fragments.map(executeFragment), new HtmlResultOutput(out))
      out.messages.toList
    }
  }
  def printer = new HtmlPrinter {}
}