package org.specs2
package matcher

import xml.NodeFunctions
import xml.NodeFunctions.*
import xml.Nodex.*
import scala.xml.*
import scala.xml.NodeSeq.*
import XPathFunctions.*
import text.Quote.*
import StringToElem.*
import collection.Seqx.*
import fp.syntax.*
import StringToElem.{given, *}
import StringMatchers.{given, *}
import execute.*, Result.*

/** The XmlMatchers trait provides matchers which are applicable to xml nodes
  */
trait XmlMatchers:

  /** match if `node` is contained anywhere inside the tested node an exact match on is required on attributes
    */
  def \\(node: Node, attributes: String*): XmlMatcher =
    deepMatch(node, attributes.toList)

  /** match if `node` is contained anywhere inside the tested node */
  def \\(node: Node): XmlMatcher =
    deepMatch(node, Nil)

  /** alias for `\\(node)` with the node label only */
  def \\(label: String, attributes: String*): XmlMatcher =
    deepMatch(label, attributes.toList)

  /** match if `node` is contained anywhere inside the tested node and has exactly the `attributeValues` as names and
    * values for its attributes
    */
  def \\(node: Node, attributeValues1: (String, String), attributeValues: (String, String)*): XmlMatcher =
    deepMatch(node, Map((attributeValues1 :: attributeValues.toList)*))

  /** alias for `\\(node, attributeValues)` with the node label only */
  def \\(label: String, attributeValues1: (String, String), attributeValues: (String, String)*): XmlMatcher =
    deepMatch(label, Map((attributeValues1 :: attributeValues.toList)*))

  /** match if `node` is the first node of the tested node an exact match on is required on attributes
    */
  def \(node: Node, attributes: String*): XmlMatcher =
    firstMatch(node, attributes.toList)

  /** match if `node` is the first node of the tested node */
  def \(node: Node): XmlMatcher =
    firstMatch(node, Nil)

  /** alias for `\(node)` with the node label only */
  def \(label: String, attributes: String*): XmlMatcher =
    firstMatch(label, attributes.toList)

  /** match if `node` is the first node of the tested node and has exactly the `attributeValues` as names and values for
    * its attributes
    */
  def \(node: Node, attributeValues1: (String, String), attributeValues: (String, String)*): XmlMatcher =
    firstMatch(node, Map((attributeValues1 :: attributeValues.toList)*))

  /** alias for `\\(node, attributeValues)` with the node label only */
  def \(label: String, attributeValues1: (String, String), attributeValues: (String, String)*): XmlMatcher =
    firstMatch(label, Map((attributeValues1 :: attributeValues.toList)*))

  /** match if `node` is equal to the tested node without testing empty text */
  def beEqualToIgnoringSpace(node: Seq[Node]): EqualIgnoringSpaceMatcher =
    new EqualIgnoringSpaceMatcher(node)

  /** alias for beEqualToIgnoringSpace */
  def be_==/(node: Seq[Node]): EqualIgnoringSpaceMatcher =
    beEqualToIgnoringSpace(node)

  /** alias for beEqualToIgnoringSpace */
  def ==/(node: Seq[Node]): EqualIgnoringSpaceMatcher =
    beEqualToIgnoringSpace(node)

  /** alias for beEqualToIgnoringSpace */
  def ==/(node: Elem): EqualIgnoringSpaceMatcher =
    beEqualToIgnoringSpace(node)

  /** alias for beEqualToIgnoringSpace */
  def be_==/(node: Elem): EqualIgnoringSpaceMatcher =
    beEqualToIgnoringSpace(node)

  /** alias for beEqualToIgnoringSpace */
  def equalToIgnoringSpace(node: Seq[Node]): EqualIgnoringSpaceMatcher =
    beEqualToIgnoringSpace(node)

  /** alias for beEqualToIgnoringSpace */
  def equalToIgnoringSpace(node: Elem): EqualIgnoringSpaceMatcher =
    beEqualToIgnoringSpace(node)

  private def deepMatch(node: Node, attributes: List[String]) =
    new XmlMatcher(Seq(new PathFunction(node, deepNodeSearch, attributes)))

  private def deepMatch(node: Node, attributes: Map[String, String]) =
    new XmlMatcher(Seq(new PathFunction(node, deepNodeSearch, attributeValues = attributes)))

  private def firstMatch(node: Node, attributes: List[String]) =
    new XmlMatcher(Seq(new PathFunction(node, firstNodeSearch, attributes)))

  private def firstMatch(node: Node, attributes: Map[String, String]) =
    new XmlMatcher(Seq(new PathFunction(node, firstNodeSearch, attributeValues = attributes)))

object XmlMatchers extends XmlMatchers

/** Matcher for equalIgnoreSpace comparison, ignoring the nodes order
  */
class EqualIgnoringSpaceMatcher(node: Seq[Node]) extends Matcher[Seq[Node]] with XmlMatcherKoMessage:
  def apply[S <: Seq[Node]](n: Expectable[S]) =
    result(NodeFunctions.isEqualIgnoringSpace(node.toList, n.value.toList), koMessage(n, node))

  def ordered = new EqualIgnoringSpaceMatcherOrdered(node)

/** Matcher for equalIgnoreSpace comparison, considering the node order
  */
class EqualIgnoringSpaceMatcherOrdered(node: Seq[Node]) extends Matcher[Seq[Node]] with XmlMatcherKoMessage:
  def apply[S <: Seq[Node]](n: Expectable[S]) =
    result(NodeFunctions.isEqualIgnoringSpaceOrdered(node.toList, n.value.toList), koMessage(n, node))

trait XmlMatcherKoMessage:
  def koMessage[S <: Seq[Node]](n: Expectable[S], node: Seq[Node]) =
    (n.description + " is not equal to " + q(node)) +
      (if n.value.toString() == node.toString then
         "\nThe nodes have the same representation but contain different elements like <n>{\"a\"} b</n> (which is <n>Text(\"a\") b</n>) and <n>a b</n>"
       else "")

/** The XmlMatcher class matches an xml Node, or a list of Nodes against a list of search functions, which can either
  * search for:<ul> <li/>a given direct child, with its label and/or attributes and/or attributes names and values
  * <li/>a given child, direct or not (maybe deeply nested), with its label and/or attributes and/or attributes names
  * and values </ul>
  *
  * XmlMatchers can be "chained" by using the \ or the \\ methods. In that case, the resulting matcher has a new search
  * function which tries to match the result of the preceding function. For example<pre> <a><b><c><d></d></c></b></a>
  * must \\("c").\("d")</pre> will be ok.
  */
case class XmlMatcher(functions: Seq[PathFunction]) extends Matcher[Seq[Node]]:
  /** do an exact match on attributes and attributes values */
  def exactly = XmlMatcher(functions.map(_.exactly))

  /** checks that the `nodes` satisfy the `functions`
    */
  def apply[S <: Seq[Node]](n: Expectable[S]) =
    val nodes = n
    val (success, okMessage, koMessage) = checkFunctions(functions, nodes.value, (true, "", ""))
    result(success, nodes.description + koMessage)

  def \(node: Node, attributeNames: String*): XmlMatcher =
    new XmlMatcher(functions :+ new PathFunction(node, firstNodeSearch, attributeNames.toList))

  def \(node: Node, attributeValues: (String, String), attributeValues2: (String, String)*): XmlMatcher =
    new XmlMatcher(
      functions :+ new PathFunction(node, firstNodeSearch, Nil, Map((attributeValues :: attributeValues2.toList)*))
    )

  def \\(node: Node, attributeNames: String*): XmlMatcher =
    new XmlMatcher(functions :+ new PathFunction(node, deepNodeSearch, attributeNames.toList))

  def \\(node: Node, attributeValues: (String, String), attributeValues2: (String, String)*): XmlMatcher =
    new XmlMatcher(
      functions :+ new PathFunction(node, deepNodeSearch, Nil, Map((attributeValues :: attributeValues2.toList)*))
    )

  /** alias for \ using the node label only */
  def \(label: String, attributeNames: String*): XmlMatcher =
    \(label.toElem, attributeNames*)

  def \(label: String, attributeValues: (String, String), attributeValues2: (String, String)*): XmlMatcher =
    \(label.toElem, attributeValues, attributeValues2*)

  /** alias for \\ using the node label only */
  def \\(label: String, attributeNames: String*): XmlMatcher =
    \\(label.toElem, attributeNames*)

  def \\(label: String, attributeValues: (String, String), attributeValues2: (String, String)*): XmlMatcher =
    \\(label.toElem, attributeValues, attributeValues2*)

  /** specify the value of the node text
    */
  def textIs(t: String): XmlMatcher =
    XmlMatcher(functions.updateLast(f => f.textIs(t)))

  /** alias for textIs */
  def \>(t: String): XmlMatcher =
    textIs(t)

  /** specify the value of the node text
    */
  def textMatches[T: MatchingExpression](t: T): XmlMatcher =
    XmlMatcher(functions.updateLast(f => f.textMatches(t)))

  /** alias for textMatches */
  def \>~[T: MatchingExpression](t: T): XmlMatcher =
    textMatches(t)

  /** checks that the `nodes` satisfy the `functions`
    * @return
    *   a MatcherResult
    */
  def checkFunctions(
      pathFunctions: Seq[PathFunction],
      nodes: Seq[Node],
      messages: (Boolean, String, String)
  ): (Boolean, String, String) =
    // check the rest of the functions, with the nodes returned by the current function
    // and build a MatcherResult being a success if the function retrieves some node
    pathFunctions match
      case search :: functions => {
        val nextNodes = search(nodes)
        val searched = search.searchedElements
        val (ok, ko) = (messages._2, messages._3)
        val (newOk, newKo) =
          (ok + " contains " + searched, ok + " doesn't contain " + searched)

        if nextNodes.isEmpty then (false, newOk, newKo)
        else checkFunctions(functions, nextNodes, (true, newOk, newKo))
      }
      case _ => messages

/** This object provides XPath functions in order to use them as parameters
  */
private[specs2] object XPathFunctions extends XPathFunctions
trait XPathFunctions:
  type XPathFunction = Function2[Node, String, NodeSeq]

  /** @return
    *   the \ XPath function
    */
  def firstNodeSearch(node: Node, label: String) = node \ label

  /** @return
    *   the \\ XPath function
    */
  def deepNodeSearch(node: Node, label: String) = node \\ label

/** The PathFunction object encapsulate a search for a node and/or attributes or attributeValues with an XPath function
  * If `node` has some children, then they are searched using equality
  */
case class PathFunction(
    val node: Node,
    val function: XPathFunction,
    val attributes: List[String] = Nil,
    val attributeValues: Map[String, String] = Map(),
    exactMatch: Boolean = false,
    textMessage: Option[String] = None,
    textMatcher: Matcher[String] = AlwaysMatcher[String]()
) extends Function1[Seq[Node], Seq[Node]]
    with XPathFunctions:

  /** @return
    *   the node if it is found and matching the searched attributes and/or attribute values when specified
    */
  def apply(nodes: Seq[Node]): Seq[Node] =
    for
      n <- nodes
      found <- function(n, node.label)
      if (found.matchNode(node, attributes, attributeValues, exactMatch, textMatcher.test))
    yield found

  def exactly: PathFunction =
    copy(exactMatch = true)

  /** add a matcher for the node text */
  def textIs(t: String): PathFunction =
    copy(textMessage = Some("with text: " + t), textMatcher = new BeEqualTo(t))

  /** add a matcher for the node text with a regular exp */
  def textMatches[T: MatchingExpression](t: T): PathFunction =
    copy(textMessage = Some("with text matching: " + t.toString), textMatcher = beMatching(t))

  /** @return
    *   "subnode" or "node" depending on the type of search a direct child search or a general search
    */
  def nodeLabel: String =
    (if !function(<a/>, "a").isEmpty then "node " else "subnode ") + q(node.label)

  /** @return
    *   a string representation of attributes or attributeValues (one of them being empty by construction)
    */
  def searchedAttributes: String =
    attributes.mkString(", ") + attributeValues.map(a => a._1 + "=\"" + a._2 + "\"").mkString(" ")

  /** @return
    *   a string representing the searched nodes, attributes, attribute values
    */
  def searchedElements: String =
    val n =
      if node.child.isEmpty then nodeLabel
      else node.toString

    val exactly = "exactly the " `orEmptyUnless` exactMatch
    val attrs =
      if attributes.isEmpty && attributeValues.isEmpty then None
      else Some("with " + exactly + "attributes: " + searchedAttributes)

    Seq(Some(n), attrs, textMessage).flatten.mkString(" ")

private[specs2] object StringToElem:

  extension (s: String) def toElem: Elem = Elem(null, s, Null, TopScope, true)

  given Conversion[String, Elem] with
    def apply(s: String): Elem =
      Elem(null, s, Null, TopScope, true)
