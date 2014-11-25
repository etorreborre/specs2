package org.specs2
package main

class SmartDiffsSpec extends Spec { def is = s2"""

 The differences between two strings must only be shown if there aren't too many of them
 ${ smart.showDiffs(a_to_z, z_to_a)._1 must not(contain("[")) }
 ${ smart.showDiffs(a_to_z, a_to_z)._1 must not(contain("[")) }
 ${ smart.showDiffs(a_to_j + m_to_z, a_to_z)._1 must contain("[") }

 The difference between two sequences must be show
   with added and missing elements in a set  $set
   with added and missing elements in a list $list
   with added and missing elements in a map $map

                                                                                                                     """

  def set =
     smart.showSeqDiffs(Seq(1, 2), Seq(1, 3), ordered = false) must_== ((Seq("2"), Seq("3")))

 def list =
  smart.showSeqDiffs(Seq(1, 2), Seq(1, 3), ordered = true) must_== ((Seq("2"), Seq("3")))

 def map = {
  val diff = "  x key = 2\n    actual value\n    3\n    expected value\n    4"
  smart.showMapDiffs(Map(1 -> 2, 2 -> 3, 3 -> 4), Map(1 -> 2, 2 -> 4, 5 -> 6)) must_== ((Seq("3 -> 4"), Seq("5 -> 6"), Seq(diff)))
 }

  val a_to_z = "abcdefghijklmnopqrstuvwxyz"
  val a_to_j = "abcdefghij"
  val m_to_z = "mnopqrstuvwxyz"
  val z_to_a = a_to_z.reverse

  val smart = SmartDiffs()
}