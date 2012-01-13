package org.specs2
package form
import DecoratedProperties._

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
trait Card {
  def title: String
  def text: String
  def toTab: Tab = Tab(title, Form.tr(TextCell(text).bkWhite))
}
