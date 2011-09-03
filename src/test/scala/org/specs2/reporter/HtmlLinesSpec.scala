package org.specs2
package reporter
import execute._
import specification._
import main._

class HtmlLinesSpec extends Specification { def is =

  "Results must be displayed with their message and icon"                                                                                   ^
    "success"                                                                                                                               ! results1^
    "failure"                                                                                                                               ! results2^
    "error"                                                                                                                                 ! results3^
    "skipped"                                                                                                                               ! results4^
    "pending"                                                                                                                               ! results5^
                                                                                                                                            end


  def results1 = results(success).toString must_==
                 <status class="ok"><div class="level0"><img src="./images/icon_success_sml.gif"></img> desc</div></status>.toString

  def results2 = results(failure) must \\(<img src="./images/icon_failure_sml.gif"></img>)

  def results3 = results(anError) must \\(<img src="./images/icon_error_sml.gif"></img>)
  
  def results4 = results(skipped).toString must_== Seq(
                <status class="ok"><div class="level0"><img src="./images/icon_skipped_sml.gif"></img> desc</div></status>,
                <status class="ok"><div class="level0"><img src="./images/icon_skipped_sml.gif"></img> skipped</div></status>).mkString("")

  def results5 = results(pending).toString must_== Seq(
    <status class="ok"><div class="level0"><img src="./images/icon_info_sml.gif"></img> desc</div></status>,
    <status class="ok"><div class="level0"><img src="./images/icon_info_sml.gif"></img> PENDING</div></status>).mkString("")

  def results(r: Result) = HtmlResult(ExecutedResult("desc", r)).print(Stats(), 0, Arguments())(new HtmlResultOutput).xml

}