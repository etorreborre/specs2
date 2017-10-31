package org.specs2
package html

import io._
import scala.xml._
import transform.RewriteRule
import org.specs2.fp._, Tree._
import xml.Nodex._
import java.net.URLDecoder
import data.UniqueNames

/**
 * This trait provide additional methods on a NodeSeq or a Node representing an html document
 */
trait Htmlx { outer =>

  implicit class HtmlOps(ns: NodeSeq) {
    def headers = outer.headers(ns)
    def headersTree = outer.headersToTree(ns.headers).toTree
    def addHeadersAnchors: NodeSeq =
      outer.headersAnchors.rewrite(ns).reduceNodes
  }

  implicit class HtmlNodeOps(n: Node) {
    def addHeadersAnchors: NodeSeq =
      outer.headersAnchors.rewrite(n).headOption.getOrElse(NodeSeq.Empty)
  }

  implicit class HtmlSeqNodeOps(ns: Seq[Node]) {
    def updateHead(f: PartialFunction[Node, Node]) = outer.updateHead(ns)(f)
    def updateHeadAttribute(name: String, value: String): NodeSeq = outer.updateHeadAttribute(ns, name, value)
    def updateHeadAttribute(name: String, value: Int): NodeSeq = outer.updateHeadAttribute(ns, name, value)
  }

  /** @return a NodeSeq where the first Node is updated with a partial function */
  def updateHead(ns: NodeSeq)(f: PartialFunction[Node, Node]) = {
    (ns.toList match {
      case (e:Node) :: rest if f.isDefinedAt(e) => f(e) :: rest
      case other                                => other
    }).reduceNodes
  }

  /** @return a NodeSeq where the first Node attribute named 'named' has a new value */
  def updateHeadAttribute(ns: NodeSeq, name: String, value: String): NodeSeq = updateHead(ns) { case (e: Elem) => e % (name -> value) }
  /** @return a NodeSeq where the first Node attribute named 'named' has a new value, from an Int */
  def updateHeadAttribute(ns: NodeSeq, name: String, value: Int): NodeSeq = updateHeadAttribute(ns, name, value.toString)

  /**
   * @return all the headers and all the subtoc elements of a document
   */
  def headers(nodes: NodeSeq): NodeSeq =
    nodes.filterNodes(isHeader)

  /** collect all the headers as a Tree */
  def headersToTree(body: NodeSeq, headers: TreeLoc[Header] = Leaf(Header()).loc): TreeLoc[Header] = {
    def goUpUntil(headers: TreeLoc[Header], level: Int): TreeLoc[Header] =
      if (headers.tree.rootLabel.level > level) headers.parent.map(goUpUntil(_, level)).getOrElse(headers)
      else headers

    lazy val currentLevel = headers.tree.rootLabel.level

    def insertHeader(eLevel: Int, e: Node, rest: NodeSeq): TreeLoc[Header] = {
      val header = Leaf(Header(eLevel, e, headers.getLabel.namer))
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
      case _                        => headers
    }
  }
  /** @return the header number if any. By convention -1 means "no header" */
  private def headerNumber(e: Node) = {
    e.label match {
      case HeaderTag(i) => Integer.valueOf(i).intValue
      case _            => -1
    }
  }

  implicit class HRef(s: String) {
    def sanitize: String = outer.sanitize(s)
    def anchorName: String = outer.anchorName(s)
  }

  /** sanitize a string so that it can be used as a href */
  def sanitize(s: String) = java.net.URLEncoder.encode(s, "UTF-8")
  /** create a sanitized anchor name */
  def anchorName(name: String) = "#"+sanitize(name)

  case class Header(level: Int = 1, node: Node = new Atom("first level"), namer: UniqueNames = uniqueNamer) {
    def name = nodeText(node)
    def isRoot = name.isEmpty

    def pandocName: String = name.toLowerCase.replace(" ", "-")
    def anchorName: String = name.anchorName
    def anchorName(baseUrl: String): String = createAnchorNameForNode(baseUrl + anchorName, namer)
  }

  implicit object HeaderShow extends Show[Header] {
    override def show(h : Header) = h.name
  }

  /** @return the text of the first child of a Node */
  def nodeText(n: Node) = <a>{n.child}</a>.text
  /** regular expression for a Header Tag */
  private val HeaderTag = "h(\\d)".r
  /** @return true if the element is a header */
  def isHeader(e: Node) = e.label.matches(HeaderTag.toString)

  /** This rule can be used to add anchors to header elements */
  def headersAnchors = {
    val namer = uniqueNamer
    rewriteRule {
      case e: Elem if isHeader(e) => <a name = {createAnchorNameForNode(nodeText(e).sanitize, namer)}>{e}</a>
    }
  }

  /** @return a rewrite rule that will rewrite recursively each node based on a partial function */
  def rewriteRule(pf: PartialFunction[Node, Seq[Node]]) = new RewriteRule {
    def applyTransformation(ns: Seq[Node]): Seq[Node] =
      if (ns.isEmpty) ns
      else            applyTransformation(ns.head) ++ applyTransformation(ns.tail)

    def applyTransformation(n: Node): Seq[Node] = n match {
      case e: Node if pf.isDefinedAt(e) => pf(e)
      case Group(xs)              => Group(applyTransformation(xs))
      case other                  =>
        val ch = n.child
        val nch = applyTransformation(ch)
        if (ch eq nch) n
        else           Elem(n.prefix, n.label, n.attributes, n.scope, true, nch: _*)
    }

    def rewrite(n: NodeSeq) = applyTransformation(n)
  }

  /** @return a unique anchor name for that node, so that 2 nodes having the same name will not direct to the same anchor in the same page */
  private def createAnchorNameForNode(text: String, namer: UniqueNames) = namer.uniqueName(text)

  /** @return a unique namer adding + and an id if a name has already been used */
  private def uniqueNamer = UniqueNames()

  /** @return the href urls in <a/> elements */
  def urls(ns: NodeSeq, filePath: FilePath = DirectoryPath.EMPTY.toFilePath): List[String] = {
    def decode(href: String) = {
      val split = href.split("#").toSeq
      val url    = filePath.dir / FilePath.unsafe(URLDecoder.decode(split(0), "UTF-8"))
      val anchor = split.drop(1).lastOption.map(anchor => "#"+anchor).getOrElse("")
      url.path + anchor
    }
    (ns \\ "a").flatMap(a => a.attribute("href").map(href => decode(href.mkString))).toList
  }

}

object Htmlx extends Htmlx
