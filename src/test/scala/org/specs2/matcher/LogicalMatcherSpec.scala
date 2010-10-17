package org.specs2
package matcher
import execute._
import specification._

class LogicalMatcherSpec extends Specification { 
  def content = 
"  a matcher can be or-ed with another one"                       ^
"    if both matches are ok the result is ok"                     ! or1^
"    if both matches are ko the result is ko"                     ! or2^
"    if the first matcher is ok, the second one is not evaluated" ! or3^
"    if both matchers are ko the combination is ko"               ! or4^
                                                                  p^
"  a matcher can be and-ed with another one"                      ^
"    if both matches are ok the result is ok"                     ! and1^
"  it is possible also to 'and' 2 match expressions"              ^
"    if both matches are ok the result is ok"                     ! and2^
                                                                  p^
"  a matcher can be ok or be skipped"                             ^
"    if it is ok, it returns a MatchSuccess result"               ! skip1^
"    if it is ko, it returns a MatchSkip result"                  ! skip2^
                                                                  end  

  def or1 = "eric" must (beMatching("e.*") or beMatching(".*c"))
  def or2 = "eric" must (beMatching("a.*") or beMatching(".*z")).not
  def or3 = "eric" must (beMatching("e.*") or beMatching({error("boom");".*z"}))
  def or4 = "eric" mustNot (beMatching("a.*") or beMatching(".*z"))

  def and1 = "eric" must be matching("a.*") and be matching(".*c")
  def and2 = ("eric" must be matching("a.*")) and ("torreborre" must be matching(".*tor.*"))
  def skip1 = 1 must be_==(1).orSkip
  def skip2 = (1 must be_==(2).orSkip).toResult must_== Skipped("'1' is not equal to '2'")
}