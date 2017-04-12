package org.specs2.text

import org.specs2.mutable.Specification
import org.specs2.specification.AllExpectations

class LinesContentDifferenceSpec extends Specification with AllExpectations { s2"""

 The LinesContentDifference class checks 2 sequences of lines

 It must display the differences if:

  1. all = true,  ordered = true  <=> lines1 are exactly the same as lines2
  2. all = true,  ordered = false <=> lines1 contains lines2 and lines2 contains lines1
  3. all = false, ordered = true  <=> lines1 contains lines2 and index(l2.1 in lines2) > index(l2.2 in lines2) =>
                                                                 index(l2.1 in lines1) > index(l2.2 in lines1)
  4. all = false, ordered = false <=> lines1 contains lines2

 One difficulty of checking the differences is also that there can be duplicated lines, so:

  * when unordered = true we must check that duplicated elements are all included

                                                                                                 """.p

  val lines1 = Seq("a", "b", "c", "d")
  val lines2 = Seq("c", "d", "b", "a")
  val lines3 = Seq("a", "b")
  val lines4 = Seq("b", "d")
  val lines5 = Seq("a", "c", "d", "c")
  val lines6 = Seq("c", "d", "c")

  val line1 = NumberedLine(1, "a")
  val line2 = NumberedLine(2, "b")
  val line3 = NumberedLine(3, "c")
  val line4 = NumberedLine(4, "d")

  "1. all = true, ordered = true" >> {
    def diff(ls1: Seq[String], ls2: Seq[String]) =
      LinesContentDifference(ls1, ls2, all = true, ordered = true)

    diff(lines1, lines2) must not(beEmpty)
    diff(lines1, lines3).show === Seq(SameLine(line1), SameLine(line2), AddedLine(line3), AddedLine(line4))
    diff(lines3, lines1).show === Seq(SameLine(line1), SameLine(line2), DeletedLine(line3), DeletedLine(line4))
    diff(lines3, lines4).show === Seq(DifferentLine(line1, line2), DifferentLine(line2, line4))
  }

  "2. all = true, ordered = false" >> {
    def diff(ls1: Seq[String], ls2: Seq[String]) =
      LinesContentDifference(ls1, ls2, all = true, ordered = false)

    diff(lines1, lines2) must beEmpty
    diff(lines1, lines3).show === Seq(SameLine(line1), SameLine(line2), AddedLine(line3), AddedLine(line4))
    diff(lines3, lines1).show === Seq(SameLine(line1), SameLine(line2), DeletedLine(line3), DeletedLine(line4))
    diff(lines3, lines4).show === Seq(AddedLine(line1), SameLine(line2), DeletedLine(line4))
  }


  "3. all = false, ordered = true" >> {
    def diff(ls1: Seq[String], ls2: Seq[String]) =
      LinesContentDifference(ls1, ls2, all = false, ordered = true)

    diff(lines1, lines2) must not(beEmpty)
    diff(lines1, lines3).show === Seq(SameLine(line1), SameLine(line2))
    diff(lines3, lines1).show === Seq(SameLine(line1), SameLine(line2), DeletedLine(line3), DeletedLine(line4))
    diff(lines3, lines4).show === Seq(SameLine(line2), DeletedLine(line4))
  }

  "4. all = false, ordered = false" >> {
    def diff(ls1: Seq[String], ls2: Seq[String]) =
      LinesContentDifference(ls1, ls2, all = false, ordered = false)

    diff(lines1, lines2) must beEmpty
    diff(lines1, lines3).show === Seq(SameLine(line1), SameLine(line2))
    diff(lines3, lines1).show === Seq(SameLine(line1), SameLine(line2), DeletedLine(line3), DeletedLine(line4))
    diff(lines3, lines4).show === Seq(SameLine(line2), DeletedLine(line4))
    diff(lines1, lines5).show === Seq(SameLine(line1), SameLine(line3), SameLine(line4), DeletedLine(line3))

  }

}
