package org.specs2
package form

import DecoratedProperties._
import specification._
import core._
import org.specs2.concurrent.ExecutionEnv

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
trait Card extends Specification with Snippets { def is = text
  implicit def executionEnv: ExecutionEnv =
    ExecutionEnv.fromGlobalExecutionContext

  def title: String
  def text: SpecStructure

  def texts: List[Fragment] =
    text.textsList

  def toTab: Tab =
    form.Tab(title, Form.tr(TextCell(texts.map(_.description.show).mkString).bkWhite))
}
