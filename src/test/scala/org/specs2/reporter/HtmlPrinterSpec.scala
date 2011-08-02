package org.specs2
package reporter
import io._
import mock._
import specification._
import matcher.DataTables

class HtmlPrinterSpec extends Specification with Mockito { outer => def is =       sequential ^
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
  "Resources"                                                                                                           ^
    "there must be a directory for css files"                                                                           ! resources().css^
    "there must be a directory for images files"                                                                        ! resources().images^
    "there must be a directory for the js tree theme files"                                                             ! resources().jstheme^
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
                                                                                                                        p^
    "A data table must"                                                                                                 ^
      "be exported as a proper html table"                                                                              ! tables().ex1^
                                                                                                                        p^
    "The statistics must"                                                                                               ^
      "be exported as a <stats> tag"                                                                                    ! stats().ex1.pendingUntilFixed^
                                                                                                                        p^
  "A linked specification"                                                                                              ^
    "must create a new file"                                                                                            ! included().e1^
    "must get an icon representing its status"                                                                          ^
      "success if everything succeeds"                                                                                  ! included().e2^
      "failure if there is a failure"                                                                                   ! included().e3^
                                                                                                                        endp^
  "A see specification"                                                                                                 ^
    "must not create a new file"                                                                                        ! seeIt().e1^
    "must get an icon representing its status"                                                                          ^
      "success if everything succeeds"                                                                                  ! seeIt().e2^
      "failure if there is a failure"                                                                                   ! seeIt().e3.pendingUntilFixed^
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
    
    def css = there was one(fs).copySpecResourcesDir(===("css"), anyString)
    def images = there was one(fs).copySpecResourcesDir(===("images"), anyString)
    def jstheme = there was one(fs).copySpecResourcesDir(===("css/themes/default"), anyString)
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
  
  case class tables() extends MockHtmlPrinter with DataTables {
    val dataTable = "a" | "b" |> 1 ! 2 | { (a, b) => success }
    val spec: Fragments = "table" ! dataTable
    def ex1 = print(spec) must \\("table")
  }

  case class stats() extends MockHtmlPrinter {
    val spec1: Fragments = "ex1" ! failure ^ end
    def ex1 = print(spec1) must \\("stats")
  }

  trait LinkedSpecifications extends MockHtmlPrinter {
    val successfulSubSpec = new Specification { def is = "ex1" ! success }
    val failedSubSpec     = new Specification { def is = "ex1" ! failure }
  }

  case class included() extends LinkedSpecifications {
    val spec1: Fragments = "ex1" ! failure ^ "a " ~ ("successfull spec", successfulSubSpec) ^ end
    val spec2: Fragments = "ex1" ! success ^ "a " ~ ("failed spec", failedSubSpec) ^ end

    def e1 = htmlLines(spec1) must have size(2)
    def e2 = print(spec1) must \\("img", "src" -> "./images/icon_success_sml.gif")
    def e3 = print(spec2) must \\("img", "src" -> "./images/icon_failure_sml.gif")
  }

  case class seeIt() extends LinkedSpecifications {
    val spec1: Fragments = "ex1" ! failure ^ "a " ~/ ("successfull spec", successfulSubSpec) ^ end
    val spec2: Fragments = "ex1" ! success ^ "a " ~/ ("failed spec", failedSubSpec) ^ end

    fs.readFile(anyString) returns "<body><stats failures=\"2\"/></body>"

    def e1 = htmlLines(spec1) must have size(1)
    def e2 = print(spec1) must \\("img", "src" -> "./images/icon_success_sml.gif")
    def e3 = print(spec2) must \\("img", "src" -> "./images/icon_failure_sml.gif")
  }

  trait MockHtmlPrinter extends FragmentExecution { outer =>
    val fs = mock[FileSystem]
    val fileWriter = new MockFileWriter {}
    val out = fileWriter.getWriter
    def printer = new HtmlPrinter {
      override lazy val fileSystem = fs
      override lazy val fileWriter = outer.fileWriter
    }

    def htmlLines(spec: Fragments) = printer.reduce(spec.fragments.map(executeFragment), HtmlLink(SpecName("spec"))).flatten.toSeq
    def print(spec: Fragments) = htmlLines(spec).head.printXml(new HtmlResultOutput).xml

    def printSpec(spec: SpecificationStructure) = {
      printer.print(spec, spec.content.fragments.map(executeFragment))
      out.messages.mkString("\n")
    }
  }
  def printer = new HtmlPrinter {}
}