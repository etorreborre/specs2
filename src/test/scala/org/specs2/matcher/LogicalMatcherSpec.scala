package org.specs2
package matcher
import execute._
import specification._

class LogicalMatcherSpec extends Specification { 
  val examples = 
  "a matcher can be or-ed with another one"^
    "if both matches are ok the result is ok" ! e1^
    "if both matches are ko the result is ko" ! e2^
    "if the first matcher is ok, the second one is not evaluated" ! e3^
    "if both matchers are ko the combination is ko" ! e4
  
  def e1 = "eric" must (beMatching("e.*") or beMatching(".*c"))
  def e2 = "eric" must (beMatching("a.*") or beMatching(".*z")).not
  def e3 = "eric" must (beMatching("e.*") or beMatching({error("boom");".*z"}))
  def e4 = "eric" mustNot (beMatching("a.*") or beMatching(".*z"))
}