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
  def processor(implicit args: Arguments) = new PegDownProcessor(args.pegdownExtensions & ~Extensions.QUOTES & ~Extensions.SMARTS, args.pegdownTimeout)
  
  /**
   * parse the markdown string and return html.
   * code tags are prettified and newlines in paragraphs are
   * transformed to <br/> tags
   */
  def toHtml(text: String, options: MarkdownOptions = MarkdownOptions())(implicit args: Arguments) = {
    (new Specs2Visitor(text, options)).toHtml(processor.parseMarkdown(text.replace("\\\\n", "\n").toCharArray))
  }

  /**
   * parse the markdown string and return html without the enclosing paragraph
   */
  def toHtmlNoPar(text: String, options: MarkdownOptions = MarkdownOptions())(implicit args: Arguments) = {
    val html = toHtml(text, options)
    if (!text.contains("\n") || text.trim.isEmpty) html.removeEnclosingXmlTag("p") else html
  }

  /**
   * parse the markdown string and return xml (unless the arguments deactivate the markdown rendering)
   */
  def toXhtml(text: String, options: MarkdownOptions = MarkdownOptions())(implicit args: Arguments): NodeSeq = {
    val html = toHtmlNoPar(text, options)
    parse(html) match {
      case Some(f) => f
      case None => scala.xml.Text(if (args.debugMarkdown) html else text)
    }
  }

  private def parse(html: String)(implicit args: Arguments) = {
    tryo(XhtmlParser(Source.fromString("<text>"+html+"</text>")).head.child)
  }
}
private[specs2]
object Markdown extends Markdown

/**
 * specialised pegdown visitor to control the rendering of code blocks
 */
case class Specs2Visitor(text: String, options: MarkdownOptions = MarkdownOptions()) extends org.pegdown.ToHtmlSerializer(new LinkRenderer) {
  override def visit(node: CodeNode) {
    printCode(node)
  }
  override def visit(node: ParaNode) {
    super.visit(node)
  }
  override def visit(node: TextNode) {
    super.visit(node)
  }

  override def visit(node: SimpleNode) {
    super.visit(node)
    if (node.getType == SimpleNode.Type.Linebreak) {
      val indent = text.drop(node.getEndIndex).takeWhile(_ == ' ').size
      (1 to indent) foreach { i => super.visit(new SimpleNode(SimpleNode.Type.Nbsp)) }
    }
  }
  override def visit(node: VerbatimNode) {
    if (!options.verbatim && node.getType.isEmpty && node.getText.contains("\n")) {
      val indents = text.split("\n").filter(_.nonEmpty).map(line => line.takeWhile(_==' ').size)
      val verbatim = node.getText.split("\n").map(line => line.trim)
      val lines = (indents zip verbatim).map { case (indent, line) => "&nbsp;"*indent + line }.mkString("<br/>")
      super.visit(new TextNode(lines))
    }
    else super.visit(new VerbatimNode(node.getText, "prettyprint"))
  }

  private def printCode(node: TextNode) {
    val text = node.getText
    if (text.contains("\n"))
      printer.print("<pre>").
        print("""<code class="prettyprint">""").
        printEncoded(text.removeFirst("\n")).
        print("</code>").
        print("</pre>")
    else
      printer.
        print("""<code class="prettyprint">""").
        printEncoded(text).
        print("</code>")
  }
}

case class MarkdownOptions(verbatim: Boolean = true)