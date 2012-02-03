package org.specs2
package html

import scala.xml._
import transform.{RewriteRule, RuleTransformer}
import xml.Nodex._
import Htmlx._
import data.Trees._
import org.specs2.internal.scalaz.Scalaz
import Scalaz._

/**
 * This trait checks for the presence of a <toc/> tag at the beginning of a xml document and replaces it
 * by a list of links to the headers of the document
 */
private[specs2]
trait TableOfContents { outer =>

  /**
   * Create a Table of contents by building a Tree of all the header elements contained into the "body" and mapping it to an <ul/> list
   *
   * The body can contain <subtoc id="something"/> elements showing where to insert sub-table of contents corresponding to linked documents
   *
   * @param body html where to extract the table of contents
   * @param url of the document linked to the root of the toc
   * @param id of the document of the root of the toc (it is used to "open" the toc list on the current specification)
   * @param subTocs map of identifier -> toc for the sub-specifications. The subtocs are inserted where the <subtoc/> tag in present in the body
   *
   * @return the toc of a document
   */
  def tocItemList(body: NodeSeq, url: String = "", id: String = "", subTocs: Map[String, NodeSeq] = Map()): NodeSeq = {
    /** @return the toc of a document by building a Tree of all the headers and mapping it to a list of <li/> */
    def headersTocItemList(body: NodeSeq, url: String = "", id: String = "", subTocs: Map[String, NodeSeq] = Map()) = {
      body.headersTree.
        bottomUp { (h: Header, s: Stream[NodeSeq]) =>
        if (h.isRoot) {
          val headers = s.flatMap(_.toSeq).reduceNodes.toList
          val headersWithId = headers match {
            case (e:Elem) :: rest => (e % ("id" -> id.toString)) :: rest
            case other            => other
          }
          headersWithId.reduceNodes
        }
        else if (h.isSubtoc) subTocs.get(h.id).getOrElse(NodeSeq.Empty) ++ s.reduceNodes
        else
          <li><a href={url+h.anchorName}>{h.name}</a>
            { <ul>{s.toSeq}</ul> }
          </li>
      }.rootLabel
    }
    headersTocItemList(headers(body), url, id, subTocs)
  }
}
private[specs2]
object TableOfContents extends TableOfContents
