package org.specs2
package reporter
import execute._
import specification._
import main._
import matcher.DataTables
import text.NoMarkup

class HtmlResultOutputSpec extends Specification with DataTables { def is =
                                                                                                                     """
  The HtmlResultOutput class build xml fragments according to the HtmlReportOutput interface.
                                                                                                                     """ ^
  "There are functions to display a description with the corresponding icon"                                         !  descriptions^
                                                                                                                     end

  def descriptions = {
    val (out, desc) = (new HtmlResultOutput, NoMarkup("desc"))
    "output"               | "xml"                                                      |>
    out.printSuccess(desc) ! """<img src="./images/icon_success_sml.gif"></img> desc""" |
    out.printFailure(desc) ! """<img src="./images/icon_failure_sml.gif"></img> desc""" |
    out.printError(desc)   ! """<img src="./images/icon_error_sml.gif"></img> desc"""   |
    out.printSkipped(desc) ! """<img src="./images/icon_skipped_sml.gif"></img> desc""" |
    out.printPending(desc) ! """<img src="./images/icon_pending_sml.gif"></img> desc""" | { (output, xml) =>
      output.xml.toString must contain(xml)
    }
  }

  type Out = HtmlReportOutput

}