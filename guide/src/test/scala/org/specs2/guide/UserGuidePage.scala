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
"""
  |### Now learn how to... {.ribbon .both-ribbon}
  |<div class="ribbon-content">
""".stripMargin

  val AndIfYouWantToKnowMore =
  """
    |### And if you want to know more {.ribbon .both-ribbon}
    |
    |<div class="ribbon-content">
  """.stripMargin

  val vid = "</div>"

}

abstract class UserGuideCard extends Card with UserGuideVariables
