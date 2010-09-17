package org.specs2
package matcher
import execute._
import specification._

class StringMatcherSpec extends Specification { outer =>
  val Fragments = 
"  a string can be matched against a pattern using beMatching" ! e1^
"  a string can be matched against a pattern using be matching" ! e2
  
  def e1 = "eric" must beMatching("e.*")
  def e2 = "eric" aka "ETO" must beMatching("e.*")
}