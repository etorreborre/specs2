package org.specs2
package html

import scala.xml._
import transform.{RewriteRule, RuleTransformer}
import scalaz.{ TreeLoc, Scalaz, Show }
import Scalaz._
import data.Trees._

private[specs2]
trait TableOfContents {
  /**
   * create anchors for each header element and add a table of content to the node if the <toc/> tag is present
   */
  def addToc(body: Node): NodeSeq = anchor.addTo(body) |> addToc

  /** sanitize a string so that it can be used as a href */
  def sanitize(s: String) = java.net.URLEncoder.encode(s, "UTF-8")
  /** create a sanitized anchor name */
  def anchorName(name: String) = "#"+sanitize(name)

  /** collect all the headers as a Tree */
  private[specs2]
  def headersToTree(body: NodeSeq, headers: TreeLoc[Header] = leaf(Header(1, "")).loc): TreeLoc[Header] = {
    def goUpUntil(headers: TreeLoc[Header], level: Int): TreeLoc[Header] =
      if (headers.tree.rootLabel.level > level) headers.parent.map(goUpUntil(_, level)).getOrElse(headers)
      else headers

    body.toList match {
      case e :: rest if isHeader(e, (_: Int) > 2) => {
        val eLevel = headerNumber(e)
        val currentLevel = headers.tree.rootLabel.level
        val header = leaf(Header(eLevel, childText(e)))
        val newHeaders = if (eLevel == currentLevel)
          headers.insertRight(header)
        else if (eLevel > currentLevel)
          headers.insertDownLast(header)
        else
          goUpUntil(headers, eLevel).insertRight(header)
        headersToTree(rest, newHeaders)
      }
      case e :: rest => headersToTree(rest, headersToTree(e.child, headers))
      case Nil       => headers
    }
  }

  private[specs2]
  case class Header(level: Int, name: String)

  implicit object HeaderShow extends Show[Header] {
    def show(h : Header) = h.name.toList
  }

  /** @return toc of a document */
  private def toc(body: NodeSeq) = {
    headersToTree(body).toTree.
    bottomUp { (h: Header, s: Stream[NodeSeq]) =>
      { if (h.name.isEmpty) NodeSeq.Empty else <li><a href={anchorName(h.name)}>{h.name}</a></li> } ++
      ( if (s.isEmpty) NodeSeq.Empty else <ul>{s.toSeq}</ul>)
    }.rootLabel
  }

  /** @return the text of the first child of a Node */
  private def childText(n: Node) = n.text//child.headOption.map(_.text).getOrElse("")
  /** @return true if the element is a header and its number satisfies a function */
  private[specs2] def isHeader(e: Node, number: Int => Boolean) = number(headerNumber(e))
  /** @return the header number if any. By convention -1 means "no header" */
  private[specs2] def headerNumber(e: Node) = {
    val Header = "h(\\d)".r
    e.label match {
      case Header(i) => Integer.valueOf(i).intValue
      case _         => -1
    }
  }

  /** This rule can be used to add anchors to header elements */
  private object anchor extends RewriteRule {
    override def transform(n: Node): Seq[Node] = n match {
      case e: Elem if isHeader(e, (_:Int) > 1) => <a name={sanitize(childText(e))}/> ++ e
      case other => other
    }
    def addTo(n: Node) = new RuleTransformer(this).apply(n)
  }

  /** This rule can be replace the toc element with a table of contents derived from the body */
  private def tableOfContents(body: Node) = new RewriteRule {
    override def transform(n: Node): Seq[Node] = n match {
      case <toc/> => toc(body)
      case other => other
    }
    def add = new RuleTransformer(this).apply(body)
  }
  private val addToc = (n: Node) => tableOfContents(n).add
}
private[specs2]
object TableOfContents extends TableOfContents
