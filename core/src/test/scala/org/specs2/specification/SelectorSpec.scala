package org.specs2
package specification

import create.DefaultFragmentFactory
import DefaultFragmentFactory._
import reporter.Reporter
import script.Specification
import core._
import process._
import execute.Result
import matcher._
import control._
import scalaz.{Tag =>_, _}, Scalaz._
import main.Arguments
import MatchersImplicits._

class SelectorSpec extends Spec with Groups with ResultMatchers with MustThrownExpectations with Expectations { def is = s2"""

 Selection by name
 =================
  + by example name

 Selection by tag
 ================
  + tagging the next fragment
  + tagging the next fragment after an empty text
  + tagging the previous fragment
  + tagging the previous fragment before an empty text
  + tagging a section of fragments, starting with the next one
  + tagging a section of fragments, starting with the previous one
  + with overlapping sections

 Selection by previous
 =====================
  + previous

"""

  "by name" - new group {
    eg := {
      val fragments = Fragments(ex("e1"), ex("e2"))
      val env = Env(arguments = Arguments.split("ex e1"))
      val executed = fragments |> DefaultSelector.select(env)

      executed.fragments must haveSize(1)
    }
  }

  "by tag" - new group {
    eg := {
      val fragments = Fragments(tag("x"), ex("e1"), ex("e2"))
      checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
    }

    eg := {
      val fragments = Fragments(tag("x"), text(" "), ex("e1"), ex("e2"))
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
                                section("x"), 
                                ex("e2"),
                                ex("e3"),
                                ex("e4"),
                                section("x"),
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
      checkSelection(fragments, "x",           expected = Seq("e2", "e3", "e4", "e5", "e6"), unexpected = Seq("e1", "e7", "e8", "e9"))
      checkSelection(fragments, "x&&y",        expected = Seq("e4", "e5", "e6"),             unexpected = Seq("e1", "e2", "e3", "e7", "e8", "e9"))
      checkSelection(fragments, Seq("x", "y"), expected = (2 to 8).map("e"+_).toSeq,         unexpected = Seq("e1", "e9"))
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

  // test methods
  def ex(desc: String) = example(desc, success)

  // expected / unexpected is in the point of view of including the tag
  def checkSelection(fragments: Fragments, tags: Seq[String], expected: Seq[String], unexpected: Seq[String]): Result = {
    includeContains(fragments, tags, expected, unexpected)
    excludeContains(fragments, tags, expected, unexpected)
  }

  def checkSelection(fragments: Fragments, tag: String, expected: Seq[String], unexpected: Seq[String]): Result =
    checkSelection(fragments, List(tag), expected, unexpected)

  def includeContains(fragments: Fragments, tags: Seq[String], expected: Seq[String], unexpected: Seq[String]): Result = {
    val env = Env(arguments = Arguments.split(s"include ${tags.mkString(",")}"))
    val executed = (fragments.contents |> DefaultSelector.filterByMarker(env)).runLog.run
    val descriptions = executed.map(_.description.toString)

    expected.foreach(e => descriptions aka "expected for include" must contain(beMatching(".*"+e+".*")))
    unexpected.foreach(e => descriptions aka "unexpected for include" must not(contain(beMatching(".*"+e+".*"))))
    ok
  }

  def excludeContains(fragments: Fragments, tags: Seq[String], unexpected: Seq[String], expected: Seq[String]): Result = {
    val env = Env(arguments = Arguments.split(s"exclude ${tags.mkString(",")}"))
    val executed = fragments |> DefaultSelector.filterByMarker(env)
    val descriptions = executed.fragments.map(_.description.toString)

    expected.foreach(e => descriptions aka "expected for exclude" must contain(beMatching(".*"+e+".*")))
    unexpected.foreach(e => descriptions aka "unexpected for exclude"  must not(contain(beMatching(".*"+e+".*"))))
    ok
  }
}
