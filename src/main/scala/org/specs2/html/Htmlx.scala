package org.specs2
package html

import scala.xml._
import transform.{RuleTransformer, RewriteRule}
import org.specs2.internal.scalaz.{ TreeLoc, Scalaz, Show }
import Scalaz._

/**
 * This trait provide additional methods on a NodeSeq or a Node representing an html document
 */
private[specs2]
trait Htmlx { outer =>

  implicit def extendHtmlNodeSeq(ns: NodeSeq) = ExtendedHtml(ns)
  case class ExtendedHtml(ns: NodeSeq) {
    def headers = outer.headers(ns)
    def headersTree = outer.headersToTree(ns).toTree
  }

  implicit def extendHtmlNode(n: Node) = ExtendedHtmlNode(n)
  case class ExtendedHtmlNode(n: Node) {
    def addHeadersAnchors = outer.headersAnchors.addTo(n)
  }
  /**
   * @return all the headers and all the subtoc elements of a document
   */
  def headers(nodes: NodeSeq): NodeSeq = nodes.filter((e: Node) => isHeader(e) || isSubtoc(e))

  /** collect all the headers as a Tree */
  def headersToTree(body: NodeSeq, headers: TreeLoc[Header] = leaf(Header()).loc): TreeLoc[Header] = {
    def goUpUntil(headers: TreeLoc[Header], level: Int): TreeLoc[Header] =
      if (headers.tree.rootLabel.level > level) headers.parent.map(goUpUntil(_, level)).getOrElse(headers)
      else headers

    lazy val currentLevel = headers.tree.rootLabel.level

    def insertHeader(eLevel: Int, e: Node, rest: NodeSeq): TreeLoc[Header] = {
      val header = leaf(Header(eLevel, e))
      val newHeaders = if (eLevel == currentLevel)
        headers.insertRight(header)
      else if (eLevel > currentLevel)
        headers.insertDownLast(header)
      else {
        val parent = goUpUntil(headers, eLevel)
        if (parent.tree.rootLabel.level == 1)
          parent.insertDownLast(header)
        else
          parent.insertRight(header)
      }
      headersToTree(rest, newHeaders)
    }

    body.toList match {
      case e :: rest if isHeader(e) => insertHeader(headerNumber(e), e, rest)
      case e :: rest if isSubtoc(e) => insertHeader(currentLevel + 1, e, rest)
      case _                        => headers
    }
  }



  implicit def href(s: String) = HRef(s)
  case class HRef(s: String) {
    def sanitize = outer.sanitize(s)
    def anchorName = outer.anchorName(s)
  }

  /** sanitize a string so that it can be used as a href */
  def sanitize(s: String) = java.net.URLEncoder.encode(s, "UTF-8")
  /** create a sanitized anchor name */
  def anchorName(name: String) = "#"+sanitize(name)

  case class Header(level: Int = 1, node: Node = new Atom("first level")) {
    def name = nodeText(node)
    def isRoot = name.isEmpty
    def isSubtoc = outer.isSubtoc(node)

    def id: String = node.attributes.get("id").map(_.toString).getOrElse("")
    def anchorName = name.anchorName
  }
  implicit object HeaderShow extends Show[Header] {
    def show(h : Header) = h.name.toList
  }

  /** @return the text of the first child of a Node, removing notoc elements */
  def nodeText(n: Node) = <a>{n.child.filterNot(_.label == "notoc")}</a>.text
  /** regular expression for a Header Tag */
  private val HeaderTag = "h(\\d)".r
  /** regular expression for a Subtoc Tag */
  private val SubtocTag = "subtoc".r
  /** @return true if the element is a header */
  def isHeader(e: Node) = e.label.matches(HeaderTag.toString)
  /** @return true if the element is a subtoc element */
  def isSubtoc(e: Node) = e.label.matches(SubtocTag.toString)
  /** @return the header number if any. By convention -1 means "no header" */
  def headerNumber(e: Node) = {
    e.label match {
      case HeaderTag(i) => Integer.valueOf(i).intValue
      case _            => -1
    }
  }

  /** This rule can be used to add anchors to header elements */
  object headersAnchors extends RewriteRule {
    override def transform(n: Node): Seq[Node] = n match {
      case e: Elem if isHeader(e) => <a name={nodeText(e).sanitize}/> ++ e
      case other                  => other
    }
    def addTo(n: Node) = new RuleTransformer(this).apply(n)
  }


}

private[specs2]
object Htmlx extends Htmlx