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
      "be printed as a div"                                                               ! fragments().text1^
      "be indented to its level with a css property"                                      ! fragments().text2^
	                                                                                      p^
    "An example must"                                                                     ^
      "have a success icon if successful"                                                 ! fragments().ex1^
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
    val spec: Fragments = "Specification".title ^ "t1" ^ "t2" ^ "ex1" ! success
    def text1 = print(spec) must \\(<div>t1</div>)
    def text2 = print(spec) must \\(<div>t2</div>, "class"->"level1")
    def ex1 = print(spec) must \\("div", "class"->"level2") \("img", "src"->"./images/icon_success_sml.gif")
  }
  
  trait MockHtmlPrinter extends FragmentExecution {
    val fs = mock[FileSystem]
    def printer = new HtmlPrinter { override lazy val fileSystem = fs }
    val out = new MockWriter {}
    
    def print(spec: Fragments) = {
      printer.reduce(spec.fragments.map(executeFragment)).
              printXml(new HtmlResultOutput(out)).xml
    }
  }
  def printer = new HtmlPrinter {}
}