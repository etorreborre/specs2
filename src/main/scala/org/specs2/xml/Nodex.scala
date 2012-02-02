package org.specs2
package xml

import scala.xml._
import collection.Iterablex._

/**
 * Extension methods for NodeSeqs and Nodes
 */
private[specs2]
trait Nodex { outer =>
  /** extend a NodeSeq */
  implicit def extendNodeSeq(ns: NodeSeq): ExtendedNodeSeq = new ExtendedNodeSeq(ns)
  /**
   * This class adds more methods to the NodeSeq class
   */
  class ExtendedNodeSeq(ns: NodeSeq) {
    def ==/(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpace(ns, n)
    def isEqualIgnoringSpace(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpace(ns, n)
    def isEqualIgnoringSpaceOrdered(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpaceOrdered(ns, n)
    def filter(condition: Node => Boolean) = NodeFunctions.filter(ns, condition)
  }
  implicit def extendNode(n: Node): ExtendedNode = new ExtendedNode(n)
  /**
   * This class adds more methods to the Node class
   */
  class ExtendedNode(n: Node) {
    /**
     * @return true if the Node represents some empty text (containing spaces or newlines)
     */
    def isSpaceNode: Boolean = NodeFunctions.isSpaceNode(n)
    def matchNode(other: Node, attributes: List[String] = Nil, attributeValues: Map[String, String] = Map(), exactMatch: Boolean = false) =
      NodeFunctions.matchNode(n, other, attributes, attributeValues, exactMatch)
  }

  implicit def reducable(ns: Seq[NodeSeq]) = new Reducable(ns)
  class Reducable(ns: Seq[NodeSeq]) {
    def reduceNodes = ns.foldLeft(NodeSeq.Empty) { (res, cur) => res ++ cur }
  }

  /**
   * reduce a sequence of T's with a function transforming T's to NodeSeq
   */
  implicit def anyReducable[T](ns: Seq[T]) = new AnyReducable(ns)
  class AnyReducable[T](ns: Seq[T]) {
    def reduceNodes(f: T => NodeSeq) = ns.foldLeft(NodeSeq.Empty) { (res, cur) => res ++ f(cur) }
  }

  /**
   * this implicit definition adds an 'unless' method to a NodeSeq so that it is only evaluated if a condition is true.
   * Otherwise NodeSeq.Empty is returned
   */
  implicit def unless(ns: =>NodeSeq): UnlessEmpty = new UnlessEmpty(ns)
  class UnlessEmpty(ns: =>NodeSeq) {
    def unless(b: Boolean) = if (b) NodeSeq.Empty else ns
  }

  /**
   * @return an unprefixed attribute from pair
   */
  implicit def pairToUnprefixedAttribute(pair: Tuple2[Any, Any]) = new UnprefixedAttribute(pair._1.toString, pair._2.toString, Null)

}
private[specs2]
object Nodex extends Nodex
