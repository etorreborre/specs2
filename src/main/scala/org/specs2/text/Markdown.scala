package org.specs2
package text

import org.pegdown.{ PegDownProcessor, Extensions }
import scala.io.Source
import scala.xml._
import parsing.XhtmlParser
import main.Arguments
import control.Exceptions._
import Trim._

/**
 * This trait can process strings formatted using the Markdown syntax and output html
 */
private[specs2]
trait Markdown {
  /**
   * @return a Markdown processor
   *         for now QUOTES and SMARTS are not rendered to avoid  <?> characters to appear on html pages
   */
  def processor(implicit args: Arguments) = new PegDownProcessor(args.report.pegdownExtensions & ~Extensions.QUOTES & ~Extensions.SMARTS)
  
  /**
   * parse the markdown string and return html.
   * code tags are prettified and newlines in paragraphs are
   * transformed to <br/> tags
   */
  def toHtml(text: String)(implicit args: Arguments) = {
    processor.markdownToHtml(text.replace("\\\\n", "\n")).
      replaceAll("<code>" -> "<code class='prettyprint'>")
  }

  /**
   * parse the markdown string and return html without the enclosing paragraph
   */
  def toHtmlNoPar(text: String)(implicit args: Arguments) = {
    if (text.trim.isEmpty) ""
    else {
      val html = toHtml(text)

      if (text.contains("\n")) s"<p>$html</p>"
      else html.removeEnclosingXmlTag("p")
    }
  }

  /**
   * parse the markdown string and return xml (unless the arguments deactivate the markdown rendering)
   */
  def toXhtml(text: String)(implicit args: Arguments): NodeSeq = {
    val html = toHtmlNoPar(text)
    parse(html) match {
      case Some(f) => f
      case None => scala.xml.Text(if (args.debugMarkdown) html else text)
    }
  }

  private def parse(html: String)(implicit args: Arguments) = {
    val f = (e: Exception) => if (args.debugMarkdown) e.printStackTrace
    tryo(XhtmlParser(Source.fromString("<text>"+html+"</text>")).head.child)(f)
  }
}
private[specs2]
object Markdown extends Markdown