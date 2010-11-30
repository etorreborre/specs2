package org.specs2
package matcher

import scala.xml._
import scala.xml.NodeSeq._
import xpath._
import text.Quote._
import org.specs2.xml.NodeFunctions._
import StringToElem._
/**
 * The <code>XmlMatchers</code> trait provides matchers which are applicable to xml nodes
 */
trait XmlMatchers extends XmlBaseMatchers with XmlBeHaveMatchers 
trait XmlBaseMatchers { outer =>
  /** dummy implicit to allow more overloading with varargs */
  trait __[+_]
  implicit object __ extends __[Any]
  
  private def deepMatch(node: Node, attributes: List[String]) =
    new XmlMatcher(List(new PathFunction(node, attributes, nodeSearch _)))
  private def deepMatch(node: Node, attributes: Map[String, String]) =
    new XmlMatcher(List(new PathFunction(node, attributes, nodeSearch _)))
  private def firstMatch(node: Node, attributes: List[String]) =
    new XmlMatcher(List(new PathFunction(node, attributes, subNodeSearch _)))
  private def firstMatch(node: Node, attributes: Map[String, String]) =
    new XmlMatcher(List(new PathFunction(node, attributes, subNodeSearch _)))
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
  def \\[_: __](node: Node, attributeValues: (String, String)*) = 
    deepMatch(node, Map(attributeValues: _*))
  /** alias for <code>\\(node, attributeValues)</code> with the node label only */   
  def \\[_: __](label: String, attributeValues: (String, String)*) =
    deepMatch(label, Map(attributeValues: _*))

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
  def \[_: __](node: Node, attributeValues: (String, String)*) = 
    firstMatch(node, Map(attributeValues: _*))
  /** alias for <code>\\(node, attributeValues)</code> with the node label only */   
  def \[_: __](label: String, attributeValues: (String, String)*) = 
    firstMatch(label, Map(attributeValues: _*))

  /** match if <code>node</code> is equal to the tested node without testing empty text */   
  def beEqualToIgnoringSpace(node: Seq[Node]) = new EqualIgnoringSpaceMatcher(node)
  /** alias for beEqualToIgnoringSpace */   
  def ==/(node: Seq[Node]): EqualIgnoringSpaceMatcher = beEqualToIgnoringSpace(node)
  /** alias for beEqualToIgnoringSpace */   
  def equalToIgnoringSpace(node: Seq[Node]) = beEqualToIgnoringSpace(node)
  /** alias for beEqualToIgnoringSpace */   
  def equalToIgnoringSpace(node: Elem) = beEqualToIgnoringSpace(node)
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
  
  /**
   * checks that the <code>nodes</code> satisfy the <code>functions</code>
   */
  def apply[S <: Seq[Node]](n: =>Expectable[S]) = {
    val nodes = n
    val r = checkFunctions(functions, nodes.value, result(true, nodes.value.toString, nodes.value.toString, Expectable(nodes.value.head)))
    result(r.isSuccess, 
           nodes.description + r.message, 
           nodes.description + r.message, nodes) 
  }
  
  /**
   * checks that the <code>nodes</code> satisfy the <code>functions</code>
   * @returns a MatcherResult (status, ok message, ko message)
   */
  def checkFunctions(pathFunctions: List[PathFunction], nodes: Seq[Node], r: MatchResult[Node]): MatchResult[Node] = {
    // return the result if we have a failure or if there are no (or no more) functions to check
    if (!r.isSuccess || pathFunctions.isEmpty) 
      return r
    
    // check the rest of the functions, with the nodes returned by the current function
    // and build a MatcherResult being a success if the function retrieves some node
    pathFunctions match {
      case function :: rest => {
         val functionResult = function(nodes) 
         val searched = searchedElements(function)
         checkFunctions(rest, 
                        functionResult, 
                        result(r.isSuccess && !functionResult.isEmpty, 
                        r.message + (if (r.message == nodes.toString) "" else " and") + " contains " + searched, 
                        r.message + (if (r.message == nodes.toString) "" else " but") + " doesn't contain " + searched,
                        r.expectable))
      }
      case _ => r
    }
  }
  
  /**
   * @returns a string representing the searched nodes, attributes, attribute values
   */
  private[this] def searchedElements(function: PathFunction) = {
      val node = if (function.node.child.isEmpty)
                   function.nodeLabel
                 else
                   function.node.toString
      val attributes = if (function.attributes.isEmpty && function.attributeValues.isEmpty) 
                         "" 
                       else 
                         " with attributes: " + function.searchedAttributes
      node + attributes
  }

  /**
   * @returns a new Matcher which will try to find <code>node</code> as a direct child after using
   * functions to find elements
   */
  def \(node: Node): XmlMatcher = new XmlMatcher(functions:::List(new PathFunction(node, Nil, subNodeSearch _)))

  /**
   * @returns a new Matcher which will try to find <code>node</code> as a child (possibly deeply nested) after 
   * using functions to find elements
   */
  def \\(node: Node): XmlMatcher = new XmlMatcher(functions:::List(new PathFunction(node, Nil, nodeSearch _)))

  /**
   * alias for \ using the node label only 
   */
  def \(label: String): XmlMatcher = \(label.toElem)

  /**
   * alias for \\ using the node label only 
   */
  def \\(label: String): XmlMatcher = \\(label.toElem)
}  

/**
 * This object provides XPath functions in order to use them as parameters
 */
object xpath extends XPathFunctions
trait XPathFunctions {
  type XPathFunction = Function2[Node, String, NodeSeq]
  /**
   * @returns the \ XPath function
   */
  def subNodeSearch(node: Node, label: String) = node \ label  

  /**
   * @returns the \\ XPath function
   */
  def nodeSearch(node: Node, label: String) = node \\ label  
}

/**
 * The PathFunction object encapsulate a search for a node and/or attributes or attributeValues with an XPath function
 * If <code>node</code> has some children, then they are searched using equality
 */
class PathFunction(val node: Node, val attributes: List[String], val attributeValues: Map[String, String], val function: XPathFunction) extends Function1[Seq[Node], Seq[Node]] with XPathFunctions {

  /**
   * @returns a PathFunction looking for a Node
   */
  def this(n: Node, function: XPathFunction) = this(n, Nil, Map.empty, function)

  /**
   * @returns a PathFunction looking for a Node and its attributes
   */
  def this(n: Node, attributes: List[String], function: XPathFunction) = this(n, attributes, Map.empty, function)

  /**
   * @returns a PathFunction looking for a Node and its attributes and attributes values
   */
  def this(n: Node, attributeValues: Map[String, String], function: XPathFunction) = this(n, Nil, attributeValues, function)

  /**
   * @returns the node if it is found and matching the searched attributes and/or attribute values when specified
   */
  def apply(nodes: Seq[Node]): Seq[Node] = for(n <- nodes;
                                                         found <- function(n, node.label) if (matchNode(found))) 
                                                       yield found 

  /**
   * @returns "subnode" or "node" depending on the type of search a direct child search or a general search
   */
  def nodeLabel: String = (if (!function(<a/>, "a").isEmpty) "node " else "subnode " )+ node.label
  
  /**
   * @returns true if the node found with a label also satisfies the attributes and/or values requirement
   */
  def matchNode(found: Node): Boolean = {
    // returns true if m matches the attribute names or attribute names + values
    def attributesMatch(m: MetaData) = {
      if (!attributes.isEmpty) {
//        if (allAttributes)
//          m.map((a: MetaData) => a.key).toList.intersect(attributes) == attributes
//        else {
          val attributesNames = m.map((a: MetaData) => a.key).toList
          attributes.forall(attributesNames.contains(_))
//        }
      }
      else if (!attributeValues.isEmpty) {
//        if (allAttributes)
//          Map(m.map((a: MetaData) => a.key -> a.value.toString).toList: _*) == attributeValues
//        else {
          val attributesNamesAndValues: Map[String, String] = Map(m.map((a: MetaData) => a.key -> a.value.toString).toList: _*)
          attributeValues.forall((pair: (String, String)) =>  attributesNamesAndValues.isDefinedAt(pair._1) && attributesNamesAndValues(pair._1) == pair._2)
//        }
      }
      else
        true
    }
    // returns true if the node matches the specified children
    def childrenMatch(n: Node) = {
      if (node.child.isEmpty) 
        true 
      else 
        isEqualIgnoringSpace(fromSeq(n.child), fromSeq(node.child))
    }

    attributesMatch(found.attributes) && childrenMatch(found) 
  }

  /**
   * @returns a string representation of attributes or attributeValues (one of them being empty by construction)
   */
  def searchedAttributes = attributes.mkString(", ") + attributeValues.map(a=> a._1 + "=\"" + a._2 + "\"").mkString(" ")
}
private[specs2] object StringToElem {
  implicit def toElement(s: String): ToElem = new ToElem(s)
  class ToElem(s: String) { def toElem: Elem = Elem(null, s, Null, TopScope) }
  implicit def toNode(s: String): Elem = Elem(null, s, Null, TopScope)
}
