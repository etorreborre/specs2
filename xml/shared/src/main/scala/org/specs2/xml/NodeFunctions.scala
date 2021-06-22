package org.specs2
package xml

import scala.xml._
import NodeSeq._
import collection.Iterablex._

/**
 * This object provides useful functions for Nodes and NodeSeqs
 */
private[specs2]
trait NodeFunctions {
  /**
   * @return true if the Node represents some empty text (containing spaces or newlines)
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
   * @return true if two Nodes are equal without considering spaces but with ordered children
   */
  def isEqualIgnoringSpaceOrdered(node: NodeSeq, n: NodeSeq): Boolean = {
    def sameOrder(nodes1: NodeSeq, nodes2: NodeSeq) = nodes1.isSimilar(nodes2, isEqualIgnoringSpaceOrdered)
    isEqualIgnoringSpace(node, n, sameOrder)
  }
  /**
   * @return true if two Nodes are equal without considering spaces
   */
  def isEqualIgnoringSpace(node: NodeSeq, n: NodeSeq): Boolean = {
    def sameAs(nodes1: NodeSeq, nodes2: NodeSeq) = nodes1.toList.sameElementsAs(nodes2, isEqualIgnoringSpace)
    isEqualIgnoringSpace(node, n, sameAs)
  }

  /**
   * @return true if two Nodes are equal without considering spaces, taking a function
   *               to apply recursively to compare children nodes
   */
  def isEqualIgnoringSpace(node: NodeSeq, n: NodeSeq, iterableComparison: Function2[NodeSeq, NodeSeq, Boolean]): Boolean = {
    def isAtom: Function[Node, Boolean] = { case (n: Atom[_]) => true; case _ => false }

    def compareChildren(n1: List[Node], n2: List[Node]) = {
      (n1.takeWhile(isAtom), n2) match {
        case (Nil, _) => iterableComparison(NodeSeq.fromSeq(n1), NodeSeq.fromSeq(n2))
        case (atoms, (n2: Text) :: rest2) =>
          atoms.mkString.trim == n2.toString.trim &&
            iterableComparison(NodeSeq.fromSeq(n1.dropWhile(isAtom)), NodeSeq.fromSeq(rest2))

        case _ => iterableComparison(NodeSeq.fromSeq(n1), NodeSeq.fromSeq(n2))
      }
    }
    (node, n) match {
      // Groups must be removed from comparisons because they throw exception when getting 'attributes' or 'children
      case (Group(node1), _)         => isEqualIgnoringSpace(node1, n, iterableComparison)
      case (_, Group(n1))            => isEqualIgnoringSpace(node, n1, iterableComparison)

      // checks for null
      case (null, other)             => other == null
      case (other, null)             => other == null

      // checks for 'leaf' types
      case (n1: Text, n2:Text)       => n1.text.trim == n2.text.trim
      case (n1: Text, n2:Atom[_])    => n1.text.trim == n2.text.trim
      case (n1: Atom[_], n2:Text)    => n1.text.trim == n2.text.trim
      case (n1: Atom[_], n2:Atom[_]) => n1.text.trim == n2.text.trim

      // general case
      case (n1: Node, n2:Node) => (isSpaceNode(n1) && isSpaceNode(n2)) ||
        n1.prefix == n2.prefix &&
          attributesSet(n1) == attributesSet(n2) &&
          n1.label == n2.label &&
          compareChildren(n1.child.toList.filter(!isSpaceNode(_)), n2.child.toList.filter(!isSpaceNode(_)))
      case (n1: NodeSeq, n2: NodeSeq) => iterableComparison(n1.filter(!isSpaceNode(_)), n2.filter(!isSpaceNode(_)))
    }
  }

  /** @return the set of attributes as a set of key/value */
  private def attributesSet(n: Node): Set[(String, String)] = n.attributes.toSet.map((n:MetaData) => (n.key, n.value.mkString(",")))

  /**
   * @return true if the node found with a label also satisfies the attributes and/or values requirement
   */
  def matchNode(node: Node,
                other: Node,
                attributes: List[String] = Nil,
                attributeValues: Map[String, String] = Map(),
                exactMatch: Boolean = false,
                textTest: String => Boolean = (s:String) => true): Boolean = {

    def attributesNamesExactMatch(m: MetaData) =
      m.map((a: MetaData) => a.key).toList.intersect(attributes) == attributes

    def attributesNamesPartialMatch(m: MetaData) = {
      val attributesNames = m.map((a: MetaData) => a.key).toList
      attributes.forall(attributesNames.contains(_))
    }

    def attributesValuesNamesExactMatch(m: MetaData) =
      Map(m.map((a: MetaData) => a.key -> a.value.toString).toList: _*) == attributeValues

    def attributesValuesNamesPartialMatch(m: MetaData) = {
      val attributesNamesAndValues: Map[String, String] = Map(m.map((a: MetaData) => a.key -> a.value.toString).toList: _*)
      attributeValues.forall((pair: (String, String)) =>  attributesNamesAndValues.isDefinedAt(pair._1) && (attributesNamesAndValues(pair._1) matches pair._2))
    }

    def attributesNamesMatch(m: MetaData) =
      attributes.isEmpty                            ||
        exactMatch && attributesNamesExactMatch(m)    ||
        !exactMatch && attributesNamesPartialMatch(m)

    def attributesValuesMatch(m: MetaData) =
      attributeValues.isEmpty                             ||
        exactMatch && attributesValuesNamesExactMatch(m)    ||
        !exactMatch && attributesValuesNamesPartialMatch(m)

    // returns true if the node matches the specified children
    def childrenMatch(n: Node) =
      other.child.isEmpty || isEqualIgnoringSpace(fromSeq(n.child), fromSeq(other.child))

    def textMatch(n: Node) = textTest(n.text)

    (node, other) match {
      // Groups must be removed from comparisons because they throw exception when getting 'attributes' or 'children'
      case (Group(node1), _)         => false
      case (_, Group(n1))            => false
      case _                         =>
        attributesNamesMatch(node.attributes) && attributesValuesMatch(node.attributes) && childrenMatch(node) && textMatch(node)
    }
  }

  /** @return all the nodes satisfying a condition as a NodeSeq */
  def filter(nodes: NodeSeq, condition: Node => Boolean, recurse: Node => Boolean = (e: Node) => true): NodeSeq = {
    nodes.toList match {
      case e :: rest if condition(e)      => e ++ filter(rest, condition, recurse)
      case (e:Elem) :: rest if recurse(e) => filter(e.child, condition, recurse) ++ filter(rest, condition, recurse)
      case e :: rest                      => filter(rest, condition, recurse)
      case Nil                            => Nil
    }
  }

}

private[specs2]
object NodeFunctions extends NodeFunctions
