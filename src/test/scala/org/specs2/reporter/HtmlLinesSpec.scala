package org.specs2
package reporter
import execute._
import specification._
import main._
import matcher.DataTables

class HtmlLinesSpec extends Specification with DataTables { def is =
                                                                                                                                            """
  HtmlLines encapsulate ExecutedFragments and produce corresponding html fragments to include into Html reports.
                                                                                                                                            """ ^
  "Results must be displayed with" ^
    "their icon and message"                                                                                                                ! icons^
                                                                                                                                            end

  def icons = {
    "result" | "display"                                                  |>
    success  ! """<img src="./images/icon_success_sml.gif"></img> desc""" |
    failure  ! """<img src="./images/icon_failure_sml.gif"></img> desc""" |
    anError  ! """<img src="./images/icon_error_sml.gif"></img> desc"""   |
    skipped  ! """<img src="./images/icon_skipped_sml.gif"></img> desc""" |
    pending  ! """<img src="./images/icon_pending_sml.gif"></img> desc""" | { (result, display) =>
      html(result).toString must contain(display)
    }
  }

  def html(r: Result) = HtmlResult(ExecutedResult("desc", r)).print(Stats(), 0, Arguments())(new HtmlResultOutput).xml

}