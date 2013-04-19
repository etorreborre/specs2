package org.specs2
package reporter

import specification._
import matcher.{XmlMatchers, DataTable, DataTables}
import text.NoMarkup
import execute.DecoratedResult

class HtmlResultOutputSpec extends Specification with DataTables with XmlMatchers { def is = s2"""
                                                                                                                        
  The HtmlResultOutput class build xml fragments according to the HtmlReportOutput interface.
                                                                                                                        
  There are functions to display a description with the corresponding icon                           $descriptions

  A link to another specification is displayed as an html link
    with a subtoc element having the specification id                                                ${links().e1}
    with a link relative to the filePath                                                             ${links().e2}

  DataTables
    A datatable must not be shown if the example is successful                                       ${dataTables().e1}
    A datatable must be displayed if there is a failure                                              ${dataTables().e2}
    A datatable used as an auto-example must be displayed                                            ${dataTables().e3}
                                                                                                     """

  def descriptions = {
    val (out, desc) = (new HtmlResultOutput, NoMarkup("desc"))
    "output"               | "xml"                                                 |>
    out.printSuccess(desc) ! """<img src="./images/icon_success_sml.gif"/> desc""" |
    out.printFailure(desc) ! """<img src="./images/icon_failure_sml.gif"/> desc""" |
    out.printError(desc)   ! """<img src="./images/icon_error_sml.gif"/> desc"""   |
    out.printSkipped(desc) ! """<img src="./images/icon_skipped_sml.gif"/> desc""" |
    out.printPending(desc) ! """<img src="./images/icon_pending_sml.gif"/> desc""" | { (output, xml) =>
      output.xml.toString must contain(xml)
    }
  }
  
  case class links() {
    val out          = new HtmlResultOutput
    val specLink     = SpecHtmlLink(SpecName("name"), "before", "link", "after", "tip")
	  val htmlSpecLink = out.printLink(specLink, 0)

    def e1 = htmlSpecLink.xml must \\("subtoc", "specId")
    def e2 = new HtmlResultOutput(filePath = "guide/MySpec.html").printLink(specLink, 0).xml must \\("a", "href" -> "../name.html")
  }

  case class dataTables() extends DataTables {
    val okTable = "a" |> 1 | { a => ok }
    val koTable = "a" |> 1 | { a => ko }

    val out = new HtmlResultOutput

    def e1 = new HtmlResult(ExecutedResult("description", okTable)).print(out).xml must not(\\("table"))
    def e2 = new HtmlResult(ExecutedResult("description", koTable)).print(out).xml must \\("table")
    def e3 = new HtmlResult(ExecutedResult("", okTable)).print(out).xml must \\("table")
  }


  type Out = HtmlReportOutput

}