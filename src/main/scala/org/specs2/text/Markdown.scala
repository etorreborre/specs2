package org.specs2
package text
import org.pegdown.PegDownProcessor
import Trim._

trait Markdown {
  lazy val processor = new PegDownProcessor
  def toHtml(text: String) = processor.markdownToHtml(text).replaceAll("<code>" -> "<code class='prettyprint'>")
  def toHtmlNoPar(text: String) = {
    val html = toHtml(text)
    if (html.removeNewLines.contains("\n")) html
    else html.removeEnclosingXmlTag("p")
  }
}
object Markdown extends Markdown