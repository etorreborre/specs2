package org.specs2
package text

import mutable.Specification

class LinesContentDifferenceSpec extends Specification {
"""
The LinesContentDifference class checks 2 sequences of lines

It must display the differences if:

 1. partial = false, unordered = false <=> lines1 are exactly the same as lines2
 2. partial = false, unordered = true  <=> lines1 contains lines2 and lines2 contains lines1
 3. partial = true,  unordered = false <=> lines1 contains lines2 and index(l2.1 in lines2) > index(l2.2 in lines2) =>
                                                                      index(l2.1 in lines1) > index(l2.2 in lines1)
 4. partial = true,  unordered = true  <=> lines1 contains lines2

""".newp

  val lines1 = Seq("a", "b", "c", "d")
  val lines2 = Seq("c", "d", "b", "a")
  val lines3 = Seq("a", "b")
  val lines4 = Seq("b", "d")


 "1. partial = false, unordered = false" >> {
   def diff(ls1: Seq[String], ls2: Seq[String]) =
     LinesContentDifference(ls1, ls2, partial = false, reportMisplaced = false, unordered = false)

   diff(lines1, lines2).isEmpty
   diff(lines1, lines3).show === (Seq("c", "d"), Seq())
   diff(lines3, lines1).show === (Seq(), Seq("c", "d"))
   diff(lines3, lines4).show === (Seq("a"), Seq("d"))
 }

}
