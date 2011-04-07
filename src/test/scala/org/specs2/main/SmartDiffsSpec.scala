package org.specs2
package main

class SmartDiffsSpec extends SpecificationWithJUnit { def is =

  "The differences between two strings must only be shown if there aren't too many of them"                             ^
   { smart.showDiffs(a_to_z, z_to_a)._1 must not contain("[") }                                                         ^
   { smart.showDiffs(a_to_z, a_to_z)._1 must not contain("[") }                                                         ^
   { smart.showDiffs(a_to_j + m_to_z, a_to_z)._1 must contain("[") }                                                    ^
                                                                                                                        end

  val a_to_z = "abcdefghijklmnopqrstuvwxyz"
  val a_to_j = "abcdefghij"
  val m_to_z = "mnopqrstuvwxyz"
  val z_to_a = a_to_z.reverse

  val smart = SmartDiffs()
}