package org.specs2
package text
import org.pegdown.PegDownProcessor
import Trim._

trait Markdown {
  lazy val processor = new PegDownProcessor
  def toHtml(text: String) = processor.markdownToHtml(text)
  def toHtmlNoPar(text: String) = {
    val html = toHtml(text)
    if (html.trimNewLines.contains("\n")) html
    else html.trimEnclosingXmlTag("p")
  }
}
object Markdown extends Markdown