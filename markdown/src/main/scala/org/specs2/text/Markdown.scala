package org.specs2
package text

import scala.io.Source
import scala.xml.NodeSeq
import scala.xml.parsing.XhtmlParser
import scala.collection.mutable.*
import scala.collection.JavaConverters.iterableAsScalaIterableConverter
import control.Exceptions.*
import Trim.*

import com.vladsch.flexmark.ast.*
import com.vladsch.flexmark.ast.util.*
import com.vladsch.flexmark.html.AttributeProvider
import com.vladsch.flexmark.html.AttributeProviderFactory
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.html.HtmlWriter
import com.vladsch.flexmark.html.IndependentAttributeProviderFactory
import com.vladsch.flexmark.html.renderer.AttributablePart
import com.vladsch.flexmark.html.renderer.DelegatingNodeRendererFactory
import com.vladsch.flexmark.html.renderer.LinkResolverContext
import com.vladsch.flexmark.html.renderer.NodeRenderer
import com.vladsch.flexmark.html.renderer.NodeRendererContext
import com.vladsch.flexmark.html.renderer.NodeRenderingHandler
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.parser.PegdownExtensions
import com.vladsch.flexmark.profile.pegdown.PegdownOptionsAdapter
import com.vladsch.flexmark.util.ast.*
import com.vladsch.flexmark.util.data.*
import com.vladsch.flexmark.util.data.MutableDataHolder
import com.vladsch.flexmark.util.data.MutableDataSet
import com.vladsch.flexmark.util.html.MutableAttributes
import com.vladsch.flexmark.util.misc.Extension
import com.vladsch.flexmark.util.sequence.Escaping

import java.util.Arrays
import java.util.Collection
import java.util.Collections
import java.util.HashSet
import java.util.Set

/**
 * This trait can process strings formatted using the Markdown syntax and output html
 */
private[specs2]
trait Markdown:

  private val pegDownOptions: DataHolder = PegdownOptionsAdapter.flexmarkOptions(
            ~PegdownExtensions.QUOTES &
            ~PegdownExtensions.SMARTS &
            ~PegdownExtensions.EXTANCHORLINKS)

  private val options: DataHolder =
    MutableDataSet.merge(pegDownOptions, MutableDataSet().set(Parser.EXTENSIONS, Arrays.asList[Extension](new Specs2Extension)))

  /**
   * @return a Markdown parser
   */
  private lazy val parser: Parser =
    Parser.builder(options).build

  /**
   * @return an HTML renderer
   * for now QUOTES and SMARTS are not rendered to avoid  <?> characters to appear on html pages
   */
  private lazy val renderer: HtmlRenderer =
    HtmlRenderer.builder(options).indentSize(2).build

  /**
   * parse the markdown string and return html.
   * code tags are prettified and newlines in paragraphs are
   * transformed to <br/> tags
   */
  def toHtml(text: String): String =
    val document = parser.parse(text.replace("\\\\n", "\n"))
    renderer.render(document)

  /**
   * parse the markdown string and return html without the enclosing paragraph
   */
  def toHtmlNoPar(text: String): String =
    val html = toHtml(text)
    if !text.contains("\n") || text.trim.isEmpty then html.trimEnd("\n").removeEnclosingXmlTag("p") else html

  /**
   * parse the markdown string and return xml (unless the arguments deactivate the markdown rendering)
   */
  def toXhtml(text: String): NodeSeq =
    val html = toHtmlNoPar(text)
    parse(html) match
      case Some(f) => f
      case None => scala.xml.Text(text)

  private def parse(html: String) =
    tryo(XhtmlParser(Source.fromString("<text>"+html+"</text>")).head.child)

private[specs2]
object Markdown extends Markdown

class Specs2AttributeProvider extends AttributeProvider:
  override def setAttributes(node: Node, part: AttributablePart, attributes: MutableAttributes): Unit =
    if node.isInstanceOf[IndentedCodeBlock] || node.isInstanceOf[Code] then
      attributes.replaceValue("class", "prettyprint")

object Specs2AttributeProvider:
  def createFactory: AttributeProviderFactory =
    new IndependentAttributeProviderFactory:
      override def apply(context: LinkResolverContext): AttributeProvider =
        Specs2AttributeProvider()

class Specs2Extension extends HtmlRenderer.HtmlRendererExtension:
  override def rendererOptions(options: MutableDataHolder) = ()

  override def extend(rendererBuilder: HtmlRenderer.Builder, rendererType: String) =
    rendererBuilder.attributeProviderFactory(Specs2AttributeProvider.createFactory)
