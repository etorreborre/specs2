package org.specs2
package xml
import scala.xml._
import collection.Listx._
import collection.Iterablex._
/**
 * This object provides useful functions for Nodes and NodeSeqs
 */
private[specs2]
trait NodeFunctions {
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
   * @returns true if two Nodes are equal without considering spaces but with ordered children
   */
  def isEqualIgnoringSpaceOrdered(node: NodeSeq, n: NodeSeq): Boolean = {
    def sameOrder(nodes1: NodeSeq, nodes2: NodeSeq) = nodes1.isSimilar(nodes2, isEqualIgnoringSpace _)
    isEqualIgnoringSpace(node, n, sameOrder(_, _))
  }
  /**
   * @returns true if two Nodes are equal without considering spaces
   */
  def isEqualIgnoringSpace(node: NodeSeq, n: NodeSeq): Boolean = {
    def sameAs(nodes1: NodeSeq, nodes2: NodeSeq) = nodes1.toList.sameElementsAs(nodes2.toSeq, isEqualIgnoringSpace _)
    isEqualIgnoringSpace(node, n, sameAs(_, _))
  }
  
  /**
   * @returns true if two Nodes are equal without considering spaces, taking a function
   *               to apply recursively to compare children nodes
   */
  def isEqualIgnoringSpace(node: NodeSeq, n: NodeSeq, iterableComparison: Function2[NodeSeq, NodeSeq, Boolean]): Boolean = {
    def isAtom: Function[Node, Boolean] = { case (n: Atom[_]) => true; case _ => false }
    def compareChildren(n1: List[Node], n2: List[Node]) = {
      (n1.takeWhile(isAtom), n2) match { 
         case (Nil, _) => iterableComparison(NodeSeq.fromSeq(n1), NodeSeq.fromSeq(n2))
         case (atoms, (n2: Text) :: rest2) => {
           atoms.mkString.trim == n2.toString.trim && iterableComparison(NodeSeq.fromSeq(n1.dropWhile(isAtom)), NodeSeq.fromSeq(rest2))
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
}
private[specs2]
object NodeFunctions extends NodeFunctions 
