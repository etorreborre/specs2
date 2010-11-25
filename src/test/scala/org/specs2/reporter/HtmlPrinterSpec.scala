package org.specs2
package reporter

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
                                                                                          end
                                                                                          
  case class filepath() {
    implicit val argument = args()
    def e1 = printer.reportPath(getClass) must startWith("target/specs-reports")
    def e2 = {
      val printer = new HtmlPrinter {
        override lazy val outputDir = "output/"
      }
      printer.reportPath(getClass) must startWith("output/")
    }
    def e3 =  printer.reportPath(getClass) must endWith(getClass.getName + ".html")
  }

  def printer = new HtmlPrinter {}
}