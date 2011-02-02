package org.specs2
package xml

import scala.xml._
import collection.Iterablex._

/**
 * Extension methods for NodeSeqs and Nodes
 */
private[specs2]
trait Nodex {
  /** extend a NodeSeq */
  implicit def extendNodeSeq(ns: NodeSeq): ExtendedNodeSeq = new ExtendedNodeSeq(ns)
  /**
   * This class adds more methods to the NodeSeq class
   */
  class ExtendedNodeSeq(ns: NodeSeq) {
    def ==/(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpace(ns, n)
    def isEqualIgnoringSpace(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpace(ns, n)
    def isEqualIgnoringSpaceOrdered(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpaceOrdered(ns, n)
  }
  implicit def extendNode(n: Node): ExtendedNode = new ExtendedNode(n)
  /**
   * This class adds more methods to the Node class
   */
  class ExtendedNode(n: Node) {
    /**
     * @returns true if the Node represents some empty text (containing spaces or newlines)
     */
    def isSpaceNode: Boolean = NodeFunctions.isSpaceNode(n)
    def matchNode(other: Node, attributes: List[String] = Nil, attributeValues: Map[String, String] = Map(), exactMatch: Boolean = false) =
      NodeFunctions.matchNode(n, other, attributes, attributeValues, exactMatch)
  }

  implicit def reducable(ns: Seq[NodeSeq]) = new Reducable(ns)
  class Reducable(ns: Seq[NodeSeq]) {
    def reduce = ns.foldLeft(NodeSeq.Empty) { (res, cur) => res ++ cur }
  }

  implicit def unless(ns: =>NodeSeq): UnlessEmpty = new UnlessEmpty(ns)
  class UnlessEmpty(ns: =>NodeSeq) {
    def unless(b: Boolean) = if (!b) ns else NodeSeq.Empty
  }
}
private[specs2]
object Nodex extends Nodex
