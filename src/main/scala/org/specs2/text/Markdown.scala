package org.specs2
package text
import org.pegdown.PegDownProcessor
import Trim._

trait Markdown {
  lazy val processor = new PegDownProcessor
  def toHtml(text: String) = processor.markdownToHtml(text)
  def toHtmlNoPar(text: String) = toHtml(text).trimEnclosingXmlTag("p")
}
object Markdown extends Markdown