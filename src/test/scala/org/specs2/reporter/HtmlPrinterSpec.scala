package org.specs2
package reporter
import io._
import mock._
import specification._

class HtmlPrinterSpec extends SpecificationWithJUnit with Mockito { def is = 
                                                                                          """
  The HtmlPrinter class is responsible for opening an html file and writing
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
                                                                                          p^
  "Resources"                                                                             ^
    "there must be a directory for css files"                                             ! resources().css^
    "there must be a directory for images files"                                          ! resources().images^
                                                                                          p^
  "Fragments"                                                                             ^
    "A text block must"                                                                   ^
      "be printed as a paragraph"                                                         ! fragments().text1^
      "have a css style"                                                                  ! fragments().text2^
                                                                                          end
                                                                                          
  implicit val argument = args()
  case class filepath() {
    def e1 = printer.reportPath(getClass) must startWith("target/specs2-reports")
    def e2 = new HtmlPrinter { override lazy val outputDir = "output/" }.reportPath(getClass) must 
             startWith("output/")
    def e3 =  printer.reportPath(getClass) must endWith(getClass.getName + ".html")
  }

  case class title() extends MockHtmlPrinter {
    val spec: Fragments = "Specification".title ^ "t1"
    def e1 = print(spec) must \\(<title>Specification</title>)
  }
  case class resources() extends MockHtmlPrinter {
    val spec: Fragments = "Specification".title ^ "t1"
    printer.print(getClass, spec.fragments.map(executeFragment))
    
    def css = there was one(fs).copySpecResourcesDir(equalTo("css"), anyString)
    def images = there was one(fs).copySpecResourcesDir(equalTo("images"), anyString)
  }
  case class fragments() extends MockHtmlPrinter {
    val spec: Fragments = "Specification".title ^ "t1"
    def text1 = print(spec) must \\(<p>t1</p>)
    def text2 = print(spec) must \\(<p>t1</p>, "class"->"text")
  }
  
  trait MockHtmlPrinter extends FragmentExecution {
    val fs = mock[FileSystem]
    def printer = new HtmlPrinter { override lazy val fileSystem = fs }
    val out = new MockWriter {}
    
    def print(spec: Fragments) = {
      printer.printElems(spec.fragments.map(executeFragment)).
              printXml(new HtmlResultOutput(out)).xml
    }
  }
  def printer = new HtmlPrinter {}
}