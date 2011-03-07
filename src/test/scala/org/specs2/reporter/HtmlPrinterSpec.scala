package org.specs2
package reporter
import io._
import mock._
import specification._

class HtmlPrinterSpec extends SpecificationWithJUnit with Mockito { outer => def is =       sequential ^
                                                                                                                        """
The HtmlPrinter class is responsible for opening an html file and writing the specification text.
                                                                                                                        """^p^
  "The file path must"                                                                                                  ^
    "use target/specs-reports as a default value for the output directory"                                              ! filepath().e1^
    "use the `outDir` system variable if set"                                                                           ! filepath().e2^
    "use class name of the specification as file name"                                                                  ! filepath().e3^
                                                                                                                        p^
  "The page title"                                                                                                      ^
    "must be the title of the specification"                                                                            ! title().e1^
                                                                                                                        p^
  "If there are children pages"                                                                                         ^
    "there must be breadcrumbs on top of the children pages"                                                            ! breadcrumbs().e1^
                                                                                                                        p^
  "Resources"                                                                                                           ^
    "there must be a directory for css files"                                                                           ! resources().css^
    "there must be a directory for images files"                                                                        ! resources().images^
                                                                                                                        p^
  "Fragments"                                                                                                           ^
    "A text block must"                                                                                                 ^
      "be printed as a div"                                                                                             ! fragments().text1^
      "be indented to its level with a css property"                                                                    ! fragments().text2^
      "be formatted as some Mockito text"                                                                               ! fragments().text3^
	                                                                                                                      p^
    "An example must"                                                                                                   ^
      "have a success icon if successful"                                                                               ! fragments().ex1^
      "show detailed failures if any"                                                                                   ! fragments().ex2^
                                                                                                                        end
                                                                                          
  implicit val argument = args()
  case class filepath() {
    def e1 = printer.reportPath("") must startWith("target/specs2-reports")
    def e2 = new HtmlPrinter { override lazy val outputDir = "output/" }.reportPath("") must
             startWith("output/")
    def e3 = SpecName(outer).url must endWith(outer.getClass.getName + ".html")
  }

  case class title() extends MockHtmlPrinter {
    val spec: Fragments = "Specification".title ^ "t1"
    def e1 = print(spec) must \\(<title>Specification</title>)
  }
  case class resources() extends MockHtmlPrinter {
    val spec: Fragments = "Specification".title ^ "t1"
    printer.print(outer, spec.fragments.map(executeFragment))
    
    def css = there was one(fs).copySpecResourcesDir(equalTo("css"), anyString)
    def images = there was one(fs).copySpecResourcesDir(equalTo("images"), anyString)
  }
  case class breadcrumbs() extends MockHtmlPrinter {
    val child = new Specification { def is = "t2" }
    val spec = new Specification { def is = "t1" ! success ^ "child" ~ ("child", child) }

    def e1 = printSpec(spec) must contain("div id=\"breadcrumbs\"")
  }
  case class fragments() extends MockHtmlPrinter {
    val spec: Fragments = "Specification".title ^ "t1" ^ "t2" ^ "ex1" ! success ^ "*ex2*" ! success ^
                          "ex2" ! { "abcdefghijklmnopqrstuvwxyz" must_== "abcdefghijklnmopqrstuvwxyz" }
    def text1 = print(spec) must \\(<div>t1</div>)
    def text2 = print(spec) must \\(<div>t2</div>, "class"->"level1")
    def text3 = print(spec) must \\(<em>ex2</em>)

    def ex1 = print(spec) must \\("div", "class"->"level2") \("img", "src"->"./images/icon_success_sml.gif")
    def ex2 = print(spec).toString must contain("details")
  }
  
  trait MockHtmlPrinter extends FragmentExecution { outer =>
    val fs = mock[FileSystem]
    val fileWriter = new MockFileWriter {}
    val out = fileWriter.getWriter
    def printer = new HtmlPrinter {
      override lazy val fileSystem = fs
      override lazy val fileWriter = outer.fileWriter
    }

    def print(spec: Fragments) = {
      printer.reduce(spec.fragments.map(executeFragment), HtmlLink(SpecName("spec"))).head.
              printXml(new HtmlResultOutput(out)).xml
    }
    def printSpec(spec: SpecificationStructure) = {
      printer.print(spec, spec.content.fragments.map(executeFragment))
      out.messages.mkString("\n")
    }
  }
  def printer = new HtmlPrinter {}
}