package org.specs2
package xml

import scala.xml._
import collection.Iterablex._

private[specs2]
trait Nodex {
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
  }
}
private[specs2]
object Nodex extends Nodex
