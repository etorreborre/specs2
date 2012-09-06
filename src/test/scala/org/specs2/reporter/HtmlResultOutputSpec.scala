package org.specs2
package reporter
import specification._
import matcher.DataTables
import text.NoMarkup

class HtmlResultOutputSpec extends Specification with DataTables { def is =
                                                                                                                        """
  The HtmlResultOutput class build xml fragments according to the HtmlReportOutput interface.
                                                                                                                        """ ^
  "There are functions to display a description with the corresponding icon"                                            !  descriptions^
                                                                                                                        p^
  "A link to another specification is displayed as an html link"                                                        ^
    "with a subtoc element having the specification id"                                                                 ! links().e1^
    "with a link relative to the filePath"                                                                              ! links().e2^
                                                                                                                        end

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

  type Out = HtmlReportOutput

}