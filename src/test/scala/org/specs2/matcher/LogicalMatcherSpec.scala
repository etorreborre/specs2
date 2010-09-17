package org.specs2
package matcher
import execute._
import specification._

class LogicalMatcherSpec extends Specification { 
  val Fragments = 
"  a matcher can be or-ed with another one"^
"    if both matches are ok the result is ok" ! or1^
"    if both matches are ko the result is ko" ! or2^
"    if the first matcher is ok, the second one is not evaluated" ! or3^
"    if both matchers are ko the combination is ko" ! or4^
p^
"  a matcher can be and-ed with another one"^
"    if both matches are ok the result is ok" ! and1
  
  def or1 = "eric" must (beMatching("e.*") or beMatching(".*c"))
  def or2 = "eric" must (beMatching("a.*") or beMatching(".*z")).not
  def or3 = "eric" must (beMatching("e.*") or beMatching({error("boom");".*z"}))
  def or4 = "eric" mustNot (beMatching("a.*") or beMatching(".*z"))

  def and1 = "eric" must be matching("a.*") and be matching(".*c")
}