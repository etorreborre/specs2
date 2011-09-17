package org.specs2
package reporter

import matcher.DataTables
import text.MarkdownHeaders._
import io.{MockFileWriter, FileSystem}
import mock._
import specification._

class HtmlLinesSpec extends Specification with Mockito { def is =

  h4> "Fragments"                                                                                                       ^
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
                                                                                                                        end

  implicit val argument = args()
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

  trait MockHtmlPrinter extends FragmentExecution with DefaultStoring { outer =>
    val fs = mock[FileSystem]
    val fileWriter = new MockFileWriter {}
    val out = fileWriter.getWriter
    override lazy val repository = mock[StatisticsRepository]

    repository.previousResult(any[SpecName], any[Example]) returns Some(success)
    repository.getStatistics(any[SpecName]) returns Some(Stats())

    def printer = new HtmlPrinter { }

    def htmlLines(spec: Fragments) = {
      val executedSpec = ExecutedSpecification(SpecName("spec"), store(args())(ExecutedSpecification(spec.fragments.map(executeFragment))).fragments)
      printer.reduce(executedSpec)
    }
    def print(spec: Fragments) = printer.sortByFile(SpecName("spec"), HtmlLink(SpecName("spec"), "", "spec"))(htmlLines(spec)).flatten.toSeq.head.printLines(new HtmlResultOutput).xml


    def printSpec(spec: SpecificationStructure) = {
      printer.print(ExecutedSpecification(spec.content.specName, spec.content.fragments.map(executeFragment)))
      out.messages.mkString("\n")
    }

    /**
     * for the failed subspec, return the statistics as failed when it is linked with "seeOnly"
     */
    override protected def storeStats = (fn: (ExecutedFragment, SpecName)) => {
      fn match {
        case (ExecutedSpecStart(start @ SpecStart(n,_,_,true), loc, st), name) if n.title == "failedSubSpec" =>
          ExecutedSpecStart(start, loc, Stats(failures = 1))
        case (fragment, name) => fragment
      }
    }

  }


}