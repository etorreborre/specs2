package org.specs2
package text
import org.pegdown.PegDownProcessor
import scala.io.Source
import scala.xml._
import parsing.XhtmlParser
import Trim._
import main.Arguments
import control.Exceptions._

/**
 * This trait can process strings formatted using the Markdown syntax and output html
 */
trait Markdown {
  def processor = new PegDownProcessor
  /**
   * parse the markdown string and return html.
   * code tags are prettified and newlines in paragraphs are
   * transformed to <br/> tags
   */
  def toHtml(text: String) = {
    processor.markdownToHtml(text).
      replaceAll("<code>" -> "<code class='prettyprint'>").
      replaceInsideTags("p", "li")("\n" -> "<br/>")
  }
  def toHtmlNoPar(text: String) = {
    val html = toHtml(text)
    if (html.removeNewLines.contains("\n")) html
    else html.removeEnclosingXmlTag("p")
  }

  def toXhtml(text: String)(implicit args: Arguments = Arguments()) = {
    if (!args.markdown) text
    else {
      val html = toHtmlNoPar(text)
      parse(html) match {
        case Some(f) => f
        case None => if (args.debugMarkdown) html else text
      }
    }
  }
  private def parse(html: String)(implicit args: Arguments) = {
    val f = (e: Exception) => if (args.debugMarkdown) e.printStackTrace
    tryo(XhtmlParser(Source.fromString("<text>"+html+"</text>")).head.child)(f)
  }
}
object Markdown extends Markdown