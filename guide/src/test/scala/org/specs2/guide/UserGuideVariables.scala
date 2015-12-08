package org.specs2
package guide

trait UserGuideVariables extends Specs2Variables {
  val specs2 = "<s2>specs2</s2>"
  val win = "<li class=\"example success\"/>"
  val warn = "<img src=\"images/icon_failure_sml.gif\"/>"
  val triple = "\"\"\""

  val NowLearnTo =
    h3Ribbon("Now learn how to...")

  val AndIfYouWantToKnowMore =
    h3Ribbon("And if you want to know more")

  val vid = "</div>"

  def h3Ribbon(text: String) =
    s"""
      |<h3 id="$text" class="ribbon both-ribbon">$text</h3>
      |
      |<div class="ribbon-content">
    """.stripMargin

}

object UserGuideVariables extends UserGuideVariables
