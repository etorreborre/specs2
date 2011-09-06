package org.specs2
package html

import scala.xml._
import transform.{RewriteRule, RuleTransformer}
import org.specs2.internal.scalaz.{ TreeLoc, Scalaz, Show }
import Scalaz._
import data.Trees._
import xml.Nodex._
/**
 * This trait checks for the presence of a <toc/> tag at the beginning of a xml document and replaces it
 * by a list of links to the headers of the document
 */
private[specs2]
trait TableOfContents {
  /**
   * create anchors for each header element and add a table of content to the node if the <toc/> tag is present
   */
  def addToc(body: Node): NodeSeq = anchor.addTo(body) |> insertToc
  def addToc(body: NodeSeq): NodeSeq = addToc(<n>{body}</n>) \ "n"

  /** sanitize a string so that it can be used as a href */
  def sanitize(s: String) = java.net.URLEncoder.encode(s, "UTF-8")
  /** create a sanitized anchor name */
  def anchorName(name: String) = "#"+sanitize(name)

  /** @return all the headers of a document */
  def headers(body: NodeSeq): NodeSeq = {
    body.toList match {
      case e :: rest if isHeader(e) => e ++ headers(rest)
      case (e:Elem) :: rest         => headers(e.child) ++ headers(rest)
      case e :: rest                => headers(rest)
      case Nil                      => Nil
    }
  }
  /** collect all the headers as a Tree */
  def headersToTree(body: NodeSeq, headers: TreeLoc[Header] = leaf(Header(1, "")).loc): TreeLoc[Header] = {
    def goUpUntil(headers: TreeLoc[Header], level: Int): TreeLoc[Header] =
      if (headers.tree.rootLabel.level > level) headers.parent.map(goUpUntil(_, level)).getOrElse(headers)
      else headers

    body.toList match {
      case e :: rest => {
        val eLevel = headerNumber(e)
        val currentLevel = headers.tree.rootLabel.level
        val header = leaf(Header(eLevel, nodeText(e)))
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
      case Nil       => headers
    }
  }

  case class Header(level: Int, name: String)
  implicit object HeaderShow extends Show[Header] {
    def show(h : Header) = h.name.toList
  }

  /** @return the toc of a document by building a Tree of all the headers and mapping it to an <ul/> list */
  def tocElements(body: NodeSeq, url: String = "", id: Int = 0, subToc: NodeSeq = NodeSeq.Empty) = headersTocElements(headers(body), url, id, subToc)

  /** @return the toc of a document by building a Tree of all the headers and mapping it to a list of <li/> */
  private def headersTocElements(body: NodeSeq, url: String = "", id: Int = 0, subToc: NodeSeq = NodeSeq.Empty) = {
    headersToTree(body).toTree.
    bottomUp { (h: Header, s: Stream[NodeSeq]) =>
      if (h.name.isEmpty) {
        val headers = s.flatMap(_.toSeq).reduceNodes.toList
        val headersWithId = headers match {
          case (e:Elem) :: rest => (e % ("id" -> id)) :: rest
          case other            => other
        }
        headersWithId.reduceNodes
      }
      else
        <li><a href={url+anchorName(h.name)}>{h.name}</a>
          { <ul>{s.toSeq ++ subToc}</ul> }
        </li>
    }.rootLabel
  }

  /** @return the toc of a document by building a Tree of all the headers and mapping it to an <ul/> list */
  def toc(body: NodeSeq, url: String = "") = headersToc(headers(body), url)

  /** @return the toc of a document by building a Tree of all the headers and mapping it to an <ul/> list */
  private def headersToc(body: NodeSeq, url: String = "") = {
    headersToTree(body).toTree.
    bottomUp { (h: Header, s: Stream[NodeSeq]) =>
      { <li id={h.name}><a href={url+anchorName(h.name)}>{h.name}</a>{ <ul>{s.toSeq}</ul> unless s.isEmpty }</li> unless h.name.isEmpty } ++
      { <ul>{s.toSeq}</ul> unless (!h.name.isEmpty) }
    }.rootLabel
  }

  /** @return the text of the first child of a Node, removing notoc elements */
  private[specs2] def nodeText(n: Node) = <a>{n.child.filterNot(_.label == "notoc")}</a>.text
  /** regular expression for a Header Tag */
  private val HeaderTag = "h(\\d)".r
  /** @return true if the element is a header and its number satisfies a function */
  private[specs2] def isHeader(e: Node) = e.label match { case HeaderTag(i) => true; case _ => false }
  /** @return the header number if any. By convention -1 means "no header" */
  private[specs2] def headerNumber(e: Node) = {
    e.label match {
      case HeaderTag(i) => Integer.valueOf(i).intValue
      case _            => -1
    }
  }

  /** This rule can be used to add anchors to header elements */
  private object anchor extends RewriteRule {
    override def transform(n: Node): Seq[Node] = n match {
      case e: Elem if isHeader(e) => <a name={sanitize(nodeText(e))}/> ++ e
      case other => other
    }
    def addTo(n: Node) = new RuleTransformer(this).apply(n)
  }

  /** This rule can replace the toc element with a table of contents derived from the body */
  private def tableOfContents(body: Node) = new RewriteRule {
    override def transform(n: Node): Seq[Node] = n match {
      case <toc/> => headersToc(headers(body).drop(1))
      case other => other
    }
    def add = new RuleTransformer(this).apply(body)
  }
  private val insertToc = (n: Node) => tableOfContents(n).add
}
private[specs2]
object TableOfContents extends TableOfContents
