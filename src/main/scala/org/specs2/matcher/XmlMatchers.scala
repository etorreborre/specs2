package org.specs2
package matcher

import scala.xml._
import scala.xml.NodeSeq._
import XPathFunctions._
import xml.Nodex._
import text.Quote._
import org.specs2.xml.NodeFunctions._
import StringToElem._

/**
 * The XmlMatchers trait provides matchers which are applicable to xml nodes
 */
trait XmlMatchers extends XmlBaseMatchers with XmlBeHaveMatchers

private[specs2]
trait XmlBaseMatchers { outer =>
  
  /** 
   * match if <code>node</code> is contained anywhere inside the tested node
   * an exact match on is required on attributes 
   */   
  def \\(node: Node, attributes: String*): XmlMatcher = deepMatch(node, attributes.toList)
  /** match if <code>node</code> is contained anywhere inside the tested node */   
  def \\(node: Node): XmlMatcher = deepMatch(node, Nil) 
  /** alias for <code>\\(node)</code> with the node label only */   
  def \\(label: String, attributes: String*) = deepMatch(label, attributes.toList)
  /** 
   * match if <code>node</code> is contained anywhere inside the tested node and has exactly the <code>attributeValues</code> 
   * as names and values for its attributes
   */   
  def \\(node: Node, attributeValues1: (String, String), attributeValues: (String, String)*) = 
    deepMatch(node, Map((attributeValues1 :: attributeValues.toList): _*))
  /** alias for <code>\\(node, attributeValues)</code> with the node label only */   
  def \\(label: String, attributeValues1: (String, String), attributeValues: (String, String)*) =
    deepMatch(label, Map((attributeValues1 :: attributeValues.toList): _*))

  /** 
   * match if <code>node</code> is the first node of the tested node
   * an exact match on is required on attributes 
   */   
  def \(node: Node, attributes: String*): XmlMatcher = firstMatch(node, attributes.toList)
  /** match if <code>node</code> is the first node of the tested node */   
  def \(node: Node): XmlMatcher = firstMatch(node, Nil) 
  /** alias for <code>\(node)</code> with the node label only */   
  def \(label: String, attributes: String*) = firstMatch(label, attributes.toList)
  /** 
   * match if <code>node</code> is the first node of the tested node and has exactly the <code>attributeValues</code> 
   * as names and values for its attributes
   */   
  def \(node: Node, attributeValues1: (String, String), attributeValues: (String, String)*) = 
    firstMatch(node, Map((attributeValues1 :: attributeValues.toList): _*))
  /** alias for <code>\\(node, attributeValues)</code> with the node label only */   
  def \(label: String, attributeValues1: (String, String), attributeValues: (String, String)*) = 
    firstMatch(label, Map((attributeValues1 :: attributeValues.toList): _*))

  /** match if <code>node</code> is equal to the tested node without testing empty text */   
  def beEqualToIgnoringSpace(node: Seq[Node]) = new EqualIgnoringSpaceMatcher(node)
  /** alias for beEqualToIgnoringSpace */   
  def ==/(node: Seq[Node]): EqualIgnoringSpaceMatcher = beEqualToIgnoringSpace(node)
  /** alias for beEqualToIgnoringSpace */   
  def equalToIgnoringSpace(node: Seq[Node]) = beEqualToIgnoringSpace(node)
  /** alias for beEqualToIgnoringSpace */   
  def equalToIgnoringSpace(node: Elem) = beEqualToIgnoringSpace(node)

  private def deepMatch(node: Node, attributes: List[String]) =
    new XmlMatcher(List(new PathFunction(node, deepNodeSearch _, attributes)))
  private def deepMatch(node: Node, attributes: Map[String, String]) =
    new XmlMatcher(List(new PathFunction(node, deepNodeSearch _, attributeValues = attributes)))
  private def firstMatch(node: Node, attributes: List[String]) =
    new XmlMatcher(List(new PathFunction(node, firstNodeSearch _, attributes)))
  private def firstMatch(node: Node, attributes: Map[String, String]) =
    new XmlMatcher(List(new PathFunction(node, firstNodeSearch _, attributeValues = attributes)))
}

private[specs2]
trait XmlBeHaveMatchers { outer: XmlBaseMatchers =>
  implicit def toXmlResultMatcher(result: MatchResult[Seq[Node]]) : XmlResultMatcher = new XmlResultMatcher(result)
  class XmlResultMatcher(result: MatchResult[Seq[Node]]) {
    def equalToIgnoringSpace(node: Seq[Node]) = result(outer.equalToIgnoringSpace(node))
    def ==/(node: Seq[Node]) = result(outer.==/(node))
  }
  implicit def toNeutralMatcherElem(result: NeutralMatcher[Any]) : NeutralMatcherElem = new NeutralMatcherElem(result)
  class NeutralMatcherElem(result: NeutralMatcher[Any]) {
    def ==/(node: Seq[Node]) = outer.==/(node) ^^ { (e: Elem) => e.toSeq }
  }
}
/**
 * Matcher for equalIgnoreSpace comparison, ignoring the nodes order
 */   
class EqualIgnoringSpaceMatcher(node: Seq[Node]) extends Matcher[Seq[Node]]  { 
  def apply[S <: Seq[Node]](other: =>Expectable[S]) = {
    val n = other
    result(isEqualIgnoringSpace(node.toList, n.value.toList), 
        n.description + " is equal to " + q(node), 
        n.description + " is not equal to " + q(node), n) 
  }
  def ordered = new EqualIgnoringSpaceMatcherOrdered(node)
}
/**
 * Matcher for equalIgnoreSpace comparison, considering the node order
 */   
class EqualIgnoringSpaceMatcherOrdered(node: Seq[Node]) extends Matcher[Seq[Node]]  { 
  def apply[S <: Seq[Node]](other: =>Expectable[S]) = {
    val n = other
    result(isEqualIgnoringSpaceOrdered(node.toList, n.value.toList), 
        n.description + " is equal to " + q(node), 
        n.description + " is not equal to " + q(node), n) 
  }
}

/**
 * The XmlMatcher class matches an xml Node, or a list of Nodes against a list of search functions, which can either search for:<ul>
 * <li/>a given direct child, with its label and/or attributes and/or attributes names and values
 * <li/>a given child, direct or not (maybe deeply nested), with its label and/or attributes and/or attributes names and values
 * </ul>
 * 
 * XmlMatchers can be "chained" by using the \ or the \\ methods. In that case, the resulting matcher has a new
 * search function which tries to match the result of the preceding function. For example<pre>
 * <a><b><c><d></d></c></b></a> must \\("c").\("d")</pre> will be ok.
*/
case class XmlMatcher(functions: List[PathFunction]) extends Matcher[Seq[Node]] {
  /** do an exact match on attributes and attributes values */
  def exactly = XmlMatcher(functions.map(_.exactly))
  /**
   * checks that the <code>nodes</code> satisfy the <code>functions</code>
   */
  def apply[S <: Seq[Node]](n: =>Expectable[S]) = {
    val nodes = n
    val (success, okMessage, koMessage) = checkFunctions(functions, nodes.value, (true, "", ""))
    result(success, 
           nodes.description + okMessage, 
           nodes.description + koMessage, nodes) 
  }

  def \(node: Node, attributeNames: String*): XmlMatcher = 
    new XmlMatcher(functions :+ new PathFunction(node, firstNodeSearch _, attributeNames.toList))
  def \(node: Node, attributeValues: (String, String), attributeValues2: (String, String)*): XmlMatcher = 
    new XmlMatcher(functions :+ new PathFunction(node, firstNodeSearch _, Nil, Map((attributeValues :: attributeValues2.toList):_*)))
  def \\(node: Node, attributeNames: String*): XmlMatcher = 
    new XmlMatcher(functions :+ new PathFunction(node, deepNodeSearch _, attributeNames.toList))
  def \\(node: Node, attributeValues: (String, String), attributeValues2: (String, String)*): XmlMatcher = 
    new XmlMatcher(functions :+ new PathFunction(node, deepNodeSearch _, Nil, Map((attributeValues :: attributeValues2.toList):_*)))
  /** alias for \ using the node label only */
  def \(label: String, attributeNames: String*): XmlMatcher = \(label.toElem, attributeNames:_*)
  def \(label: String, attributeValues: (String, String), attributeValues2: (String, String)*): XmlMatcher = 
    \(label.toElem, attributeValues, attributeValues2:_*)
  /** alias for \\ using the node label only */
  def \\(label: String, attributeNames: String*): XmlMatcher = \\(label.toElem, attributeNames:_*)
  def \\(label: String, attributeValues: (String, String), attributeValues2: (String, String)*): XmlMatcher = 
    \\(label.toElem, attributeValues, attributeValues2:_*)
  
  /**
   * checks that the <code>nodes</code> satisfy the <code>functions</code>
   * @return a MatcherResult
   */
  def checkFunctions(pathFunctions: List[PathFunction], nodes: Seq[Node], messages: (Boolean, String, String)): (Boolean, String, String) = {
    // check the rest of the functions, with the nodes returned by the current function
    // and build a MatcherResult being a success if the function retrieves some node
    pathFunctions match {
      case search :: functions => {
         val nextNodes = search(nodes)
         val searched = search.searchedElements
         val (ok, ko) = (messages._2, messages._3)
         val (newOk, newKo) = 
             (ok + " contains " + searched, 
              ok + " doesn't contain " + searched)
              
         if (nextNodes.isEmpty) (false, newOk, newKo)
         else checkFunctions(functions, nextNodes, (true, newOk, newKo))
      }
      case _ => messages
    }
  }
}  

/**
 * This object provides XPath functions in order to use them as parameters
 */
private[specs2]
object XPathFunctions extends XPathFunctions
trait XPathFunctions {
  type XPathFunction = Function2[Node, String, NodeSeq]
  /**
   * @return the \ XPath function
   */
  def firstNodeSearch(node: Node, label: String) = node \ label  

  /**
   * @return the \\ XPath function
   */
  def deepNodeSearch(node: Node, label: String) = node \\ label  
}

/**
 * The PathFunction object encapsulate a search for a node and/or attributes or attributeValues with an XPath function
 * If <code>node</code> has some children, then they are searched using equality
 */
class PathFunction(val node: Node, val function: XPathFunction, val attributes: List[String] = Nil, val attributeValues: Map[String, String] = Map(), exactMatch: Boolean = false) extends Function1[Seq[Node], Seq[Node]] with XPathFunctions {

  /**
   * @return the node if it is found and matching the searched attributes and/or attribute values when specified
   */
  def apply(nodes: Seq[Node]): Seq[Node] = 
    for { n     <- nodes
          found <- function(n, node.label) if (found.matchNode(node, attributes, attributeValues, exactMatch)) } 
    yield found 

  def exactly = new PathFunction(node, function, attributes, attributeValues, true)
  /**
   * @return "subnode" or "node" depending on the type of search a direct child search or a general search
   */
  def nodeLabel: String = (if (!function(<a/>, "a").isEmpty) "node " else "subnode " )+ q(node.label)

  /**
   * @return a string representation of attributes or attributeValues (one of them being empty by construction)
   */
  def searchedAttributes = attributes.mkString(", ") + attributeValues.map(a=> a._1 + "=\"" + a._2 + "\"").mkString(" ")
  
  /**
   * @return a string representing the searched nodes, attributes, attribute values
   */
  def searchedElements = {
    val n = if (node.child.isEmpty) nodeLabel
            else node.toString
    val attrs = if (attributes.isEmpty && attributeValues.isEmpty) "" 
                else " with attributes: " + searchedAttributes
    n + attrs
  }

}
private[specs2] object StringToElem {
  implicit def toElement(s: String): ToElem = new ToElem(s)
  class ToElem(s: String) { def toElem: Elem = Elem(null, s, Null, TopScope) }
  implicit def toNode(s: String): Elem = Elem(null, s, Null, TopScope)
}
