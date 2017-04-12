package org.specs2
package text

import mutable.Specification
import specification.AllExpectations

class LinesContentDifferenceSpec extends Specification with AllExpectations { s2"""
 The LinesContentDifference class checks 2 sequences of lines

 It must display the differences if:

  1. partial = false, unordered = false <=> lines1 are exactly the same as lines2
  2. partial = false, unordered = true  <=> lines1 contains lines2 and lines2 contains lines1
  3. partial = true,  unordered = false <=> lines1 contains lines2 and index(l2.1 in lines2) > index(l2.2 in lines2) =>
                                                                       index(l2.1 in lines1) > index(l2.2 in lines1)
  4. partial = true,  unordered = true  <=> lines1 contains lines2

 One difficulty of checking the differences is also that there can be duplicated lines, so:

  * when unordered = true we must check that duplicated elements are all included

                                                                                                 """

  val lines1 = Seq("a", "b", "c", "d")
  val lines2 = Seq("c", "d", "b", "a")
  val lines3 = Seq("a", "b")
  val lines4 = Seq("b", "d")
  val lines5 = Seq("a", "c", "d", "c")
  val lines6 = Seq("c", "d", "c")

  "1. partial = false, unordered = false, reportMisplaced = true" >> {
   def diff(ls1: Seq[String], ls2: Seq[String]) =
     LinesContentDifference(ls1, ls2, partial = false, unordered = false, reportMisplaced = true)

   diff(lines1, lines2).isEmpty
   diff(lines1, lines3).show === Seq(MissingLine("c", 3), MissingLine("d", 4)) -> Seq()
   diff(lines3, lines1).show === Seq() -> Seq(MissingLine("c", 3), MissingLine("d", 4))
   diff(lines3, lines4).show === Seq(MissingLine("a", 1), MisplacedLine("b", 2)) -> Seq(MissingLine("d", 2))
 }

  "2. partial = false, unordered = true, reportMisplaced = true" >> {
    def diff(ls1: Seq[String], ls2: Seq[String]) =
      LinesContentDifference(ls1, ls2, partial = false, unordered = true, reportMisplaced = true)

    diff(lines1, lines2).isEmpty
    diff(lines1, lines3).show === Seq(NotFoundLine("c", 3), NotFoundLine("d", 4)) -> Seq()
    diff(lines3, lines1).show === Seq() -> Seq(NotFoundLine("c", 3), NotFoundLine("d", 4))
    diff(lines3, lines4).show === Seq(NotFoundLine("a", 1)) -> Seq(NotFoundLine("d", 2))
    diff(lines1, lines5).show === Seq(NotFoundLine("b", 2)) -> Seq(NotFoundLine("c", 4))
  }

  "3. partial = true, unordered = false, reportMisplaced = true" >> {
    def diff(ls1: Seq[String], ls2: Seq[String]) =
      LinesContentDifference(ls1, ls2, partial = true, unordered = false, reportMisplaced = true)

    diff(lines1, lines2).isEmpty
    diff(lines1, lines3).show === Seq() -> Seq()
    diff(lines3, lines1).show === Seq() -> Seq(MissingLine("c", 3), MissingLine("d", 4))
    diff(lines3, lines4).show === Seq() -> Seq(MissingLine("d", 2))
  }

  "4. partial = true, unordered = true, reportMisplaced = true" >> {
    def diff(ls1: Seq[String], ls2: Seq[String]) =
      LinesContentDifference(ls1, ls2, partial = true, unordered = true, reportMisplaced = true)

    diff(lines1, lines2).isEmpty
    diff(lines1, lines3).show === Seq() -> Seq()
    diff(lines3, lines1).show === Seq() -> Seq(NotFoundLine("c", 3), NotFoundLine("d", 4))
    diff(lines3, lines4).show === Seq() -> Seq(NotFoundLine("d", 2))
    diff(lines1, lines5).show === Seq() -> Seq(NotFoundLine("c", 4))

  }

}
