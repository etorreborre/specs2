package org.specs2
package xml
import collection.Iterablex._
import scala.xml._

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
/**
 * This object provides useful functions for Nodes and NodeSeqs
 */
object NodeFunctions {
  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isSpaceNode(n1: Node): Boolean = n1 match {
    case g: Group => false
    case _ => n1.label.equals("#PCDATA") && n1.text.matches("\\s*")
  }

  /**
   * Alias for isEqualIgnoringSpace
   */
  def ==/(node: NodeSeq, n: NodeSeq): Boolean = isEqualIgnoringSpace(node, n)

  /**
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isEqualIgnoringSpaceOrdered(node: NodeSeq, n: NodeSeq): Boolean = {
    def sameOrder(nodes1: NodeSeq, nodes2: NodeSeq) = nodes1.isSimilar(nodes2, isEqualIgnoringSpace _)
    isEqualIgnoringSpace(node, n, sameOrder(_, _))
  }
  /**
   * Generic version of This version don't check if the nodes are in the same order
   * @returns true if the Node represents some empty text (containing spaces or newlines)
   */
  def isEqualIgnoringSpace(node: NodeSeq, n: NodeSeq): Boolean = {
    def sameAs(nodes1: NodeSeq, nodes2: NodeSeq) = nodes1.toList.sameElementsAs(nodes2.toSeq, isEqualIgnoringSpace _)
    isEqualIgnoringSpace(node, n, sameAs(_, _))
  }
  def isEqualIgnoringSpace(node: NodeSeq, n: NodeSeq, iterableComparison: Function2[NodeSeq, NodeSeq, Boolean]): Boolean = {
    def compareChildren(n1: List[Node], n2: List[Node]) = {
      (n1, n2) match { 
         case ((t1: Atom[_]) :: (t2: Atom[_]) :: rest, (n2: Text) :: rest2) => {
           (t1.toString.trim + t2.toString.trim) == n2.toString.trim && iterableComparison(NodeSeq.fromSeq(rest), NodeSeq.fromSeq(rest2))
         }
         case _ => iterableComparison(NodeSeq.fromSeq(n1), NodeSeq.fromSeq(n2))
      } 
    }
    (node, n) match {
      case (null, other) => other == null
      case (other, null) => other == null
      case (n1: Text, n2:Text) => n1.text.trim == n2.text.trim
      case (n1: Text, n2:Atom[_]) => n1.text.trim == n2.text.trim
      case (n1: Atom[_], n2:Text) => n1.text.trim == n2.text.trim
      case (n1: Node, n2:Node) => (isSpaceNode(n1) && isSpaceNode(n2)) ||
                                  n1.prefix == n2.prefix && 
                                  n1.attributes.toSet == n2.attributes.toSet && 
                                  n1.label == n2.label &&
                                  compareChildren(n1.child.toList.filter(!isSpaceNode(_)), n2.child.toList.filter(!isSpaceNode(_)))
      case (n1: NodeSeq, n2: NodeSeq) => iterableComparison(n1.filter(!isSpaceNode(_)), n2.filter(!isSpaceNode(_)))
    }
  }
  def reduce[T](list: Seq[T], f: T => NodeSeq): NodeSeq = {
    if (list.isEmpty)
      NodeSeq.Empty
    else if (list.size == 1)
      f(list(0))
    else
      f(list(0)) ++ reduce(list.drop(1), f)
  }
  /** reduce a list with a function and an init NodeSeq value. */
  def fold[T](initValue: NodeSeq)(list: Iterable[T], f: T => NodeSeq): NodeSeq = {
    list.foldLeft(initValue)( (res, value) => res ++ f(value))
  }
}
object Nodex extends Nodex