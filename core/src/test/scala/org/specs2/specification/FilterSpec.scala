package org.specs2
package specification

import create.DefaultFragmentFactory
import DefaultFragmentFactory._
import specification.script.Specification
import execute.Result
import matcher.{ThrownExpectations, ResultMatchers}
import scalaz.stream.{Process, Process1}
import Process.{Env =>_,_}
import control._
import scalaz.{Tag =>_, _}, Scalaz._
import main.Arguments
import specification.core._
import specification.process._

class FilterSpec extends Specification with Groups with ResultMatchers with ThrownExpectations { def is = s2"""

 Filter by name
 ==============
  + by example name

 Filter by tag
 =============
  + tagging the next fragment
  + tagging the previous fragment
  + tagging a section of fragments, starting with the next one
  + tagging a section of fragments, starting with the previous one
  + with overlapping sections

 Filter by previous
 ==================
  + previous

"""

  "by name" - new group {
    eg := {
      val fragments = Fragments(ex("e1"), ex("e2"))
      val env = Env(arguments = Arguments("ex e1"))
      val executed = fragments |> Filter.filter(env)

      executed.fragments must haveSize(1)
    }
  }

  "by tag" - new group {
    eg := {
      val fragments = Fragments(Tag("x"), ex("e1"), ex("e2"))
      checkFiltering(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
    }

    eg := {
      val fragments = Fragments(ex("e1"), TaggedAs("x"), ex("e2"))
      checkFiltering(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
    }

    eg := {
      val fragments = Fragments(ex("e1"), 
                                Section("x"), 
                                ex("e2"),
                                ex("e3"),
                                ex("e4"),
                                Section("x"),
                                ex("e5")
      )
      checkFiltering(fragments, "x", expected = Seq("e2", "e3", "e4"), unexpected = Seq("e1", "e5"))
    }

    eg := {
      val fragments = Fragments(ex("e1"),
        ex("e2"), AsSection("x"),
        ex("e3"),
        ex("e4"), AsSection("x"),
        ex("e5")
      )
      checkFiltering(fragments, "x", expected = Seq("e2", "e3", "e4"), unexpected = Seq("e1", "e5"))
    }

    eg := {
      val fragments = Fragments(
        ex("e1"),
        ex("e2"), AsSection("x"),
        ex("e3"),
        ex("e4"), AsSection("y"),
        ex("e5"),
        ex("e6"), AsSection("x"),
        ex("e7"),
        ex("e8"), AsSection("y"),
        ex("e9")
      )
      checkFiltering(fragments, "x",           expected = Seq("e2", "e3", "e4", "e5", "e6"), unexpected = Seq("e1", "e7", "e8", "e9"))
      checkFiltering(fragments, "x&&y",        expected = Seq("e4", "e5", "e6"),             unexpected = Seq("e1", "e2", "e3", "e7", "e8", "e9"))
      checkFiltering(fragments, Seq("x", "y"), expected = (2 to 8).map("e"+_).toSeq,         unexpected = Seq("e1", "e9"))
    }
  }

  "by previous" - new group {
    eg := {
      val repo = StatisticsRepository.memory
      val env = Env(arguments = Arguments("was x"), statisticsRepository = repo)

      repo.storeResult(getClass.getName, RawText("e1"), org.specs2.execute.Failure("failed")).runOption

      val fragments = Fragments(
        ex("e1"),
        ex("e2")
      ).update(SpecStructure.withPreviousResult(getClass.getName, env))

      check(fragments, expected = Seq("e1"), unexpected = Seq("e2"))(env)
    }


    def check(fragments: Fragments, expected: Seq[String], unexpected: Seq[String])(env: Env): Result = {
      val executed = fragments |> Filter.filterByPrevious(env)
      val descriptions = executed.fragments.map(_.description.toString)

      expected.foreach(e => descriptions aka "expected for exclude" must contain(beMatching(".*"+e+".*")))
      unexpected.foreach(e => descriptions aka "unexpected for exclude"  must not contain(beMatching(".*"+e+".*")))
      ok
    }

  }

  // test methods
  def ex(desc: String) = Example(desc, success)

  // expected / unexpected is in the point of view of including the tag
  def checkFiltering(fragments: Fragments, tags: Seq[String], expected: Seq[String], unexpected: Seq[String]): Result = {
    includeContains(fragments, tags, expected, unexpected)
    excludeContains(fragments, tags, expected, unexpected)
  }

  def checkFiltering(fragments: Fragments, tag: String, expected: Seq[String], unexpected: Seq[String]): Result =
    checkFiltering(fragments, List(tag), expected, unexpected)

  def includeContains(fragments: Fragments, tags: Seq[String], expected: Seq[String], unexpected: Seq[String]): Result = {
    val env = Env(arguments = Arguments(s"include ${tags.mkString(",")}"))
    val executed = (fragments.contents |> Filter.filterByTags(env)).runLog.run
    val descriptions = executed.map(_.description.toString)

    expected.foreach(e => descriptions aka "expected for include" must contain(beMatching(".*"+e+".*")))
    unexpected.foreach(e => descriptions aka "unexpected for include" must not contain(beMatching(".*"+e+".*")))
    ok
  }

  def excludeContains(fragments: Fragments, tags: Seq[String], unexpected: Seq[String], expected: Seq[String]): Result = {
    val env = Env(arguments = Arguments(s"exclude ${tags.mkString(",")}"))
    val executed = fragments |> Filter.filterByTags(env)
    val descriptions = executed.fragments.map(_.description.toString)

    expected.foreach(e => descriptions aka "expected for exclude" must contain(beMatching(".*"+e+".*")))
    unexpected.foreach(e => descriptions aka "unexpected for exclude"  must not contain(beMatching(".*"+e+".*")))
    ok
  }
}
