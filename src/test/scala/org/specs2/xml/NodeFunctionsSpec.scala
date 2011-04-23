package org.specs2
package xml
import NodeFunctions._
import Nodex._

class NodeFunctionsSpec extends Specification { def is = sequential^
  "Node functions".title                                                                                                ^
                                                                                                                        p^
  "The matchNode function must return true if"                                                                          ^
    "there is a match on the node label"                                                                                ! e1^
    "and a match on one attribute name"                                                                                 ! e2^
    "and a match on a list of attribute names"                                                                          ! e3^
    "and a match on some attribute names and values"                                                                    ! e4^
    "with exactMatch = true, it must return true if"                                                                    ^
      "there is a match on the node label"                                                                              ! e5^
      "and a match on all attribute names"                                                                              ! e6^
      "and a match on all attribute names and values"                                                                   ! e7^
                                                                                                                        end

  def e1 = <a/>.matchNode(<a/>)                                                                                          
  def e2 = <a n="v" n2="v2"/>.matchNode(<a/>, List("n"))                                                                                          
  def e3 = <a n="v" n2="v2"/>.matchNode(<a/>, List("n", "n2"))                                                                                          
  def e4 = <a n="v" n2="v2"/>.matchNode(<a/>, attributeValues = Map("n" -> "v"))                                                                                          
                                                                                          
  def e5 = <a/>.matchNode(<a/>, exactMatch = true)                                                                                          
  def e6 = <a n="v" n2="v2"/>.matchNode(<a/>, List("n", "n2"), exactMatch = true)                                                                                          
  def e7 = <a n="v" n2="v2"/>.matchNode(<a/>, attributeValues = Map("n" -> "v", "n2" -> "v2"), exactMatch = true)                                                                                          
}