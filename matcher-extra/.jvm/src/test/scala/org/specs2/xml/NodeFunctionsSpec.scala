package org.specs2
package xml

import Nodex.*
import matcher.XmlMatchers

class NodeFunctionsSpec extends Spec with XmlMatchers {
  def is = s2"""

  The matchNode function must return true if
    there is a match on the node label                                                               $matchNode1
    and a match on one attribute name                                                                $matchNode2
    and a match on a list of attribute names                                                         $matchNode3
    and a match on some attribute names and values                                                   $matchNode4
    nodes are not groups                                                                             $matchNode5
    with exactMatch = true, it must return true if
      there is a match on the node label                                                             $exactMatch1
      and a match on all attribute names                                                             $exactMatch2
      and a match on all attribute names and values                                                  $exactMatch3

  The equalIgnoreSpace function must
    return false if 2 nodes are not equal after evaluation
    ${<a>{"a"}</a> must not(==/(<a>{"b"}</a>))}
    return true if 2 nodes are equal even with spaces
    ${<a>{"a"}</a> must ==/(<a>{" a "}</a>)}
    return true if 2 nodes are in a Group"
    ${<u>{scala.xml.Group(<a>{"a"}</a>)}</u> must ==/(<u><a>{" a "}</a></u>)}

  The equalIgnoreSpaceOrdered function must
    return false if 2 nodes are not in order
  ${NodeFunctions.isEqualIgnoringSpaceOrdered(<t><a></a><b></b></t>, <t><a></a><b></b></t>)}
  ${!NodeFunctions.isEqualIgnoringSpaceOrdered(<t><a></a><b></b></t>, <t><b></b><a></a></t>)}

"""

  def matchNode1 = <a/> matchNode <a/>
  def matchNode2 = <a n="v" n2="v2"/>.matchNode(<a/>, List("n"))
  def matchNode3 = <a n="v" n2="v2"/>.matchNode(<a/>, List("n", "n2"))
  def matchNode4 = <a n="v" n2="v2"/>.matchNode(<a/>, attributeValues = Map("n" -> "v"))
  def matchNode5 = !scala.xml.Group(<a/>).matchNode(scala.xml.Group(<a/>), List())

  def exactMatch1 = <a/>.matchNode(<a/>, exactMatch = true)
  def exactMatch2 = <a n="v" n2="v2"/>.matchNode(<a/>, List("n", "n2"), exactMatch = true)
  def exactMatch3 =
    <a n="v" n2="v2"/>.matchNode(<a/>, attributeValues = Map("n" -> "v", "n2" -> "v2"), exactMatch = true)

}
