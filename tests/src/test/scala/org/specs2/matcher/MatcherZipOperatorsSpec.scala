package org.specs2
package matcher

class MatcherZipOperatorsSpec extends Specification { def is = s2"""

 It is possible to create matchers for tuples using individual matchers for fields $e1
 The failure message must mention the full tuple and the failing field             $e2
   for a deeply nested field                                                       $e3
"""

  type S = (String, Double)
  type T = (String, String, String, Seq[S])

  val t1: T = ("a", "b", "c", Seq(("d", 1.01), ("e", 2.02)))
  val t2: T = ("a", "b", "c", Seq(("d", 1.00), ("e", 2.00)))
  val t3: T = ("z", "b", "c", Seq(("d", 1.00), ("e", 3.00)))
  val t4: T = ("a", "b", "c", Seq(("d", 1.00), ("e", 3.00)))

  // create a matcher by zipping matchers to the expected value
  def beMatching(expected: T) = expected.zip(startWith, ===, ===, _.contain(_.zip(===, ==~)).inOrder)

  /** type inference doesn't work if this matcher, specialised to Double, is not defined */
  def ==~(d: =>Double) = beCloseTo(d +/- 0.1)

  def e1 = t1 must beMatching(t2)
  def e2 = (t1 must beMatching(t3)) returns "For (z,b,c,List((d,1.0), (e,3.0)))\n  field _1: 'a' doesn't start with 'z'"
  def e3 = (t1 must beMatching(t4)) returns
    "For (a,b,c,List((d,1.0), (e,3.0)))\n"+
    "  field _4: List((d,1.01), (e,2.02)) does not contain exactly 2 correct values in order\n"+
    "For (e,3.0)\n"+
    "  field _2: '2.02' is not close to 3.0 +/- 0.1"

}
