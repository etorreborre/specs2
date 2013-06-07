package org.specs2
package text

import org.pegdown._
import ast._
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
  def toHtml(text: String, options: MarkdownOptions = MarkdownOptions())(implicit args: Arguments) = {
    (new Specs2Visitor(options)).toHtml(processor.parseMarkdown(text.replace("\\\\n", "\n").toCharArray))
  }

  /**
   * parse the markdown string and return html without the enclosing paragraph
   */
  def toHtmlNoPar(text: String, options: MarkdownOptions = MarkdownOptions())(implicit args: Arguments) = {
    if (text.trim.isEmpty) "<br/>"*(text.filter(_ == '\n').drop(1).size)
    else {
      val html = toHtml(text, options)
      if (!text.contains("\n") || text.trim.isEmpty) html.removeEnclosingXmlTag("p") else html
    }
  }

  /**
   * parse the markdown string and return xml (unless the arguments deactivate the markdown rendering)
   */
  def toXhtml(text: String, options: MarkdownOptions = MarkdownOptions())(implicit args: Arguments): NodeSeq = {
    val lines = if (text.trim.nonEmpty && text.contains("\n")) text.split("\n").map { line =>
      val (start, end) = line.span(_ == ' ')
      "&nbsp;"*start.size + end.mkString
    }.mkString("\n") else text

    val paragraph = if (!text.trim.isEmpty && text.filter(_ == '\n').size > 1 && text.reverse.dropWhile(_ == ' ').startsWith("\n")) lines.removeLast("\n") else lines
    val html = toHtmlNoPar(paragraph, options)
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

/**
 * specialised pegdown visitor to control the rendering of code blocks
 */
case class Specs2Visitor(options: MarkdownOptions = MarkdownOptions()) extends org.pegdown.ToHtmlSerializer(new LinkRenderer) {
  override def visit(node: CodeNode) {
    printTagAndAttribute(node, "code", "class", "prettyprint")
  }

  override def visit(node: VerbatimNode) {
    if (!options.verbatim && node.getType.isEmpty && node.getText.contains("\n")) {
      super.visit(new TextNode(node.getText))
    }
    else super.visit(new VerbatimNode(node.getText, "prettyprint"))
  }

  private def printTagAndAttribute(node: TextNode, tag: String, attributeName: String, attributeValue: String) {
    printer.print('<').print(tag)
    printer.print(' ').print(attributeName).print('=').print('"').print(attributeValue).print('"')
    printer.print('>')
    printer.printEncoded(node.getText())
    printer.print('<').print('/').print(tag).print('>')
  }
}

case class MarkdownOptions(verbatim: Boolean = true)