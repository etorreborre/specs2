package org.specs2
package xml

import scala.xml._

/**
 * Extension methods for NodeSeqs and Nodes
 */
private[specs2]
trait Nodex { outer =>
  /**
   * This class adds more methods to the NodeSeq class
   */
  implicit class extendNodeSeq(ns: NodeSeq) {
    def ==/(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpace(ns, n)
    def isEqualIgnoringSpace(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpace(ns, n)
    def isEqualIgnoringSpaceOrdered(n: NodeSeq): Boolean = NodeFunctions.isEqualIgnoringSpaceOrdered(ns, n)
    def filterNodes(condition: Node => Boolean, recurse: Node => Boolean = (e: Node) => true) = NodeFunctions.filter(ns, condition, recurse)
  }
  /**
   * This class adds more methods to the Node class
   */
  implicit class extendNode(n: Node) {
    /**
     * @return true if the Node represents some empty text (containing spaces or newlines)
     */
    def isSpaceNode: Boolean = NodeFunctions.isSpaceNode(n)
    def matchNode(other: Node,
                  attributes: List[String] = Nil,
                  attributeValues: Map[String, String] = Map(),
                  exactMatch: Boolean = false,
                  textTest: String => Boolean = (s:String) => true) =
      NodeFunctions.matchNode(n, other, attributes, attributeValues, exactMatch, textTest)
  }

  implicit class reducable(ns: Seq[NodeSeq]) {
    def reduceNodes = ns.foldLeft(NodeSeq.Empty) { (res, cur) => res ++ cur }
  }

  /**
   * reduce a sequence of T's with a function transforming T's to NodeSeq
   */
  implicit class anyReducable[T](ns: Seq[T]) {
    def reduceNodes(f: T => NodeSeq) = ns.foldLeft(NodeSeq.Empty) { (res, cur) => res ++ f(cur) }
  }

  /**
   * this implicit definition adds an 'unless' method to a NodeSeq so that it is only evaluated if a condition is true.
   * Otherwise NodeSeq.Empty is returned
   */
  implicit class unless(ns: =>NodeSeq) {
    def unless(b: Boolean) = if (b) NodeSeq.Empty else ns
  }

  /**
   * @return an unprefixed attribute from pair
   */
  implicit def pairToUnprefixedAttribute(pair: Tuple2[Any, Any]) = new UnprefixedAttribute(pair._1.toString, pair._2.toString, Null)

}
private[specs2]
object Nodex extends Nodex
