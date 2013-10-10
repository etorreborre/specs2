package org.specs2
package form

import DecoratedProperties._
import specification.{Fragments, Snippets}

/**
 * A set of tabs with a title, where each tab simply contains some text
 */
trait Cards {
  def title: String
  def cards: Seq[Card]
  def toTabs = Form(title).tabs(cards)((card: Card) => Tabs(Seq(card.toTab)))
}

/**
 * This trait defines a simple tab with a title and some text.
 *
 * The text will be interpreted as Markdown text when rendered as html
 */
trait Card extends Specification with Snippets { def is = ""
  def title: String
  def text: Fragments
  def toTab: Tab = Tab(title, Form.tr(TextCell(text.texts.map(_.t).mkString).bkWhite))
}
