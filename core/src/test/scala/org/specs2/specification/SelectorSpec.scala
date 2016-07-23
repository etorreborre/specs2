package org.specs2
package specification

import core._
import process._
import execute.Result
import matcher._
import control._
import scalaz.{Tag =>_, _}, Scalaz._
import main.Arguments

class SelectorSpec extends script.Specification with Groups with ResultMatchers { def is = s2"""

 Selection by name
 =================
  + by example name
  + when the name is some code

 Selection by tag
 ================
  + tagging the next fragment
  + tagging the next fragment after an empty text
  + tagging the previous fragment
  + tagging the previous fragment before an empty text
  + tagging a section of fragments, starting with the next one
  + tagging a section of fragments, starting with the previous one
  + tagging a section of fragments, starting with the previous one and a blank text
  + with overlapping sections
  + with non-overlapping sections
  + formatting fragments and empty text must not be filtered out

 Selection by previous
 =====================
  + previous


 Support Functions
 =================
  + swap empty text and before marker
  + transform before markers to after markers
  + transform tags to sections
"""
  import ff._

  "by name" - new group {
    eg := {
      val fragments = Fragments(ex("e1"), ex("e2"))
      val env = Env(arguments = Arguments.split("ex e1"))
      val executed = fragments |> DefaultSelector.select(env)

      executed.fragments must haveSize(1)
    }
    eg := {
      val fragments = Fragments(code("e1"), code("e2"))
      val env = Env(arguments = Arguments.split("ex e1"))
      val executed = fragments |> DefaultSelector.select(env)

      executed.fragments must haveSize(1)
    }
  }

  "by tag" - new group {
    eg := {
      val fragments = Fragments(ff.tag("x"), ex("e1"), ex("e2"))
      checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
    }

    eg := {
      val fragments = Fragments(ff.tag("x"), text(" "), ex("e1"), ex("e2"))
      checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
    }

    eg := {
      val fragments = Fragments(ex("e1"), taggedAs("x"), ex("e2"))
      checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
    }

    eg := {
      val fragments = Fragments(ex("e1"), text(" "), taggedAs("x"), ex("e2"))
      checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
    }

    eg := {
      val fragments = Fragments(ex("e1"),
                                ff.section("x"),
                                ex("e2"),
                                ex("e3"),
                                ex("e4"),
                                ff.section("x"),
                                ex("e5")
      )
      checkSelection(fragments, "x", expected = Seq("e2", "e3", "e4"), unexpected = Seq("e1", "e5"))
    }

    eg := {
      val fragments = Fragments(ex("e1"),
        ex("e2"), asSection("x"),
        ex("e3"),
        ex("e4"), asSection("x"),
        ex("e5")
      )
      checkSelection(fragments, "x", expected = Seq("e2", "e3", "e4"), unexpected = Seq("e1", "e5"))
    }

    eg := {
      val fragments = Fragments(
        asSection("x"),
        ff.text(" "),
        ex("e1"), asSection("x"),
        ex("e2")
      )
      checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
    }

    eg := {
      val fragments = Fragments(
        ex("e1"),
        ex("e2"), asSection("x"),
        ex("e3"),
        ex("e4"), asSection("y"),
        ex("e5"),
        ex("e6"), asSection("x"),
        ex("e7"),
        ex("e8"), asSection("y"),
        ex("e9")
      )
      checkSelection(fragments, "x",           expected = Seq("e2", "e3", "e4", "e5", "e6"), unexpected = Seq("e1", "e7", "e8", "e9")) and
      checkSelection(fragments, "x&&y",        expected = Seq("e4", "e5", "e6"),             unexpected = Seq("e1", "e2", "e3", "e7", "e8", "e9")) and
      checkSelection(fragments, Seq("x", "y"), expected = (2 to 8).map("e"+_).toSeq,         unexpected = Seq("e1", "e9"))
    }

    eg := {
      val fragments = Fragments(
        ex("e1"),
        ff.section("x"),
        ex("e2"),
        ex("e3"),
        ff.section("x"),
        ff.section("y"),
        ex("e4"),
        ex("e5"),
        ff.section("y"),
        ex("e6")
      )

      checkSelection(fragments, "x",           expected = Seq("e2", "e3"),             unexpected = Seq("e1", "e4", "e5", "e6")) and
      checkSelection(fragments, "x&&y",        expected = Seq(),                       unexpected = Seq("e1", "e2", "e3", "e4", "e5", "e6")) and
      checkSelection(fragments, Seq("x", "y"), expected = Seq("e2", "e3", "e4", "e5"), unexpected = Seq("e1", "e6"))
    }

    eg := {
      val fragments = Fragments(
        text("  "),
        ex("e1"),
        ff.tag("x"),
        ex("e2"),
        ex("e3"),
        ff.break,
        ff.tab,
        ff.tag("x"),
        ex("e4")
      )
      filterIncluded(fragments, Seq("x")) ==== Vector(
        text("  "),
        ex("e2"),
        ff.break,
        ff.tab,
        ex("e4")
      )
    }

  }

  "by previous" - new group {
    eg := {
      val repo = StatisticsRepository.memory
      val env = Env(arguments = Arguments.split("was x")).setStatisticRepository(repo)

      repo.storeResult(getClass.getName, Text("e1"), org.specs2.execute.Failure("failed")).runOption

      val fragments = Fragments(
        ex("e1"),
        ex("e2")
      ).flatMap(Statistics.readStats(getClass.getName, env))

      check(fragments, expected = Seq("e1"), unexpected = Seq("e2"))(env)
    }


    def check(fragments: Fragments, expected: Seq[String], unexpected: Seq[String])(env: Env): Result = {
      val executed = fragments |> DefaultSelector.filterByPrevious(env)
      val descriptions = executed.fragments.map(_.description.toString)

      expected.foreach(e => descriptions aka "expected for exclude" must contain(beMatching(".*"+e+".*")))
      unexpected.foreach(e => descriptions aka "unexpected for exclude"  must not(contain(beMatching(".*"+e+".*"))))
      ok
    }

  }

  "support" - new group {
    eg := {
      val original = Fragments(ex("e1"), ex("e2"), text(" "), taggedAs("t1"))
      val swapped = original |> DefaultSelector.swapBeforeMarkerAndEmptyText
      swapped.fragments.map(_.description.show) must_== Fragments(ex("e1"), ex("e2"),
        ff.tag("t1"), text(" ")).fragments.map(_.description.show)
    }
    eg := {
      val original = Fragments(ex("e1"), ex("e2"), taggedAs("t1"))
      val swapped = original |> DefaultSelector.transformBeforeMarkersToAfterMarkers
      swapped.fragments.map(_.description.show) must_==
        Fragments(ex("e1"), ff.tag("t1"), ex("e2")).fragments.map(_.description.show)
    }
    eg := {
      val original = Fragments(ex("e1"), tag("t1"), ex("e2"))
      val swapped = original |> DefaultSelector.transformTagsToSections
      swapped.fragments.map(_.description.show) must_==
        Fragments(ex("e1"), ff.section("t1"), ex("e2"), ff.section("t1")).fragments.map(_.description.show)
    }
  }

  // test methods
  def ex(desc: String) = example(desc, success)
  def code(desc: String) = example(Code(desc), success)

  // expected / unexpected is in the point of view of including the tag
  def checkSelection(fragments: Fragments, tags: Seq[String], expected: Seq[String], unexpected: Seq[String]): Result = {
    includeContains(fragments, tags, expected, unexpected) and
    excludeContains(fragments, tags, expected, unexpected)
  }

  def checkSelection(fragments: Fragments, tag: String, expected: Seq[String], unexpected: Seq[String]): Result =
    checkSelection(fragments, List(tag), expected, unexpected)

  def includeContains(fragments: Fragments, tags: Seq[String], expected: Seq[String], unexpected: Seq[String]): Result = {
    val executed = filterIncluded(fragments, tags)
    val descriptions = executed.map(_.description.toString)

    s"${descriptions.mkString(",")} contains ${expected.mkString(",")} but not ${unexpected.mkString(",")} for tags ${tags.mkString(",")}" ==> {
      Result.foreach(expected)  (e => descriptions aka "expected for include" must contain(beMatching(".*"+e+".*"))) and
      Result.foreach(unexpected)(e => descriptions aka "unexpected for include" must not(contain(beMatching(".*"+e+".*"))))
    }
  }

  def excludeContains(fragments: Fragments, tags: Seq[String], unexpected: Seq[String], expected: Seq[String]): Result = {
    val executed = filterExcluded(fragments, tags)
    val descriptions = executed.fragments.map(_.description.show)

    s"${descriptions.mkString(",")} does not contain ${unexpected.mkString(",")} but contains ${expected.mkString(",")} for tags ${tags.mkString(",")}" ==> {
      Result.foreach(expected)  (e => descriptions aka "expected for exclude" must contain(beMatching(".*"+e+".*"))) and
      Result.foreach(unexpected)(e => descriptions aka "unexpected for exclude"  must not(contain(beMatching(".*"+e+".*"))))
    }
  }

  def filterIncluded(fragments: Fragments, tags: Seq[String]) = {
    val env = Env(arguments = Arguments.split(s"include ${tags.mkString(",")}"))
    (fragments.contents |> DefaultSelector.filterByMarker(env)).runLog.run
  }

  def filterExcluded(fragments: Fragments, tags: Seq[String]) = {
    val env = Env(arguments = Arguments.split(s"exclude ${tags.mkString(",")}"))
    fragments |> DefaultSelector.filterByMarker(env)
  }

  def show(fs: Fragments): String =
    fs.fragments.map(_.description).mkString("\n")
}
