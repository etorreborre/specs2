package org.specs2
package xml

import scala.xml._
import fp._

/**
 * Extension methods for NodeSeqs and Nodes
 */
trait Nodex:

  extension (ns: NodeSeq)
    def ==/(n: NodeSeq): Boolean =
      NodeFunctions.isEqualIgnoringSpace(ns, n)

    def isEqualIgnoringSpace(n: NodeSeq): Boolean =
      NodeFunctions.isEqualIgnoringSpace(ns, n)

    def isEqualIgnoringSpaceOrdered(n: NodeSeq): Boolean =
      NodeFunctions.isEqualIgnoringSpaceOrdered(ns, n)

    def filterNodes(condition: Node => Boolean, recurse: Node => Boolean = (e: Node) => true) =
      NodeFunctions.filter(ns, condition, recurse)

  /**
   * This class adds more methods to the Node class
   */
  extension (n: Node)
    /**
     * @return true if the Node represents some empty text (containing spaces or newlines)
     */
    def isSpaceNode: Boolean =
      NodeFunctions.isSpaceNode(n)

    def matchNode(other: Node,
                  attributes: List[String] = Nil,
                  attributeValues: Map[String, String] = Map(),
                  exactMatch: Boolean = false,
                  textTest: String => Boolean = (s:String) => true): Boolean =
      NodeFunctions.matchNode(n, other, attributes, attributeValues, exactMatch, textTest)

  extension (ns: Seq[NodeSeq])
    def reduceNodes: NodeSeq =
      ns.flatMap(_.theSeq).reduceNodesWith(identity)

  /**
   * reduce a sequence of T's with a function transforming T's to NodeSeq
   */
  extension [T](ns: Seq[T])
    def reduceNodesWith(f: T => NodeSeq): NodeSeq =
      ns.foldLeft(NodeSeq.Empty) { (res, cur) => res ++ f(cur) }

  given Monoid[NodeSeq] with
    val zero: NodeSeq =
      NodeSeq.Empty

    def append(ns1: NodeSeq, ns2: =>NodeSeq): NodeSeq =
      ns1 ++ ns2

  /** @return an unprefixed attribute from pair */
  given Conversion[(Any, Any), UnprefixedAttribute] with
    def apply(pair: (Any, Any)): UnprefixedAttribute =
      new UnprefixedAttribute(pair._1.toString, pair._2.toString, Null)


object Nodex extends Nodex
