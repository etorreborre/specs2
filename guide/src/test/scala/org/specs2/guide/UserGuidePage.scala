package org.specs2
package guide

import form.Card
import specification.core.Fragments
import specification._
/**
 * base class for creating specs2 user guide pages.
 */
abstract class UserGuidePage extends Specification with UserGuideVariables with Snippets {
  override def map(fs: =>Fragments) = super.map(fs.compact)

  val NowLearnTo =
    h3Ribbon("Now learn how to...")

  val AndIfYouWantToKnowMore =
    h3Ribbon("And if you want to know more")

  val vid = "</div>"

  def h3Ribbon(text: String) =
   s"""
      |### $text {.ribbon .both-ribbon}
      |
      |<div class="ribbon-content">
    """.stripMargin
}

abstract class UserGuideCard extends Card with UserGuideVariables
