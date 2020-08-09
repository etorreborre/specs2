package org.specs2
package specification

import core._
import process._
import control._
import producer._, Producer._
import execute.Result
import main.Arguments
import org.specs2.concurrent.ExecutionEnv

class SelectorSpec(ee: ExecutionEnv) extends Specification { def is = s2"""

Selection by name
=================
 by example name $byName1
 when the name is some code $byName2
 $br

Selection by tag
================
 tagging the next fragment $byTag1
 tagging the next fragment after an empty text $byTag2
 tagging the previous fragment $byTag3
 tagging the previous fragment before an empty text $byTag4
 tagging a section of fragments, starting with the next one $byTag5
 tagging a section of fragments, starting with the previous one $byTag6
 tagging a section of fragments, starting with the previous one and a blank text $byTag7
 with overlapping sections $byTag8
 with non-overlapping sections $byTag9
 markers must be filtered out $byTag10
 $br

Selection by previous
=====================
 previous $byPrevious
 $br

Support Functions
=================
 swap empty text and before marker $support1
 transform before markers to after markers $support2
 transform tags to sections $support3

"""

  val ff = fragmentFactory
  import ff._

  def byName1 = {
    val fragments = Fragments(ex("e1"), ex("e2"))
    val arguments = Arguments.split("ex e1")
    val executed = fragments |> DefaultSelector(arguments).select(arguments)
   executed.fragmentsList(ee) must haveSize(1)
  }

  def byName2 = {
    val fragments = Fragments(code("e1"), code("e2"))
    val arguments = Arguments.split("ex e1")
    val executed = fragments |> DefaultSelector(arguments).select(arguments)
   executed.fragmentsList(ee) must haveSize(1)
  }

  def byTag1 = {
    val fragments = Fragments(ff.tag("x"), ex("e1"), ex("e2"))
    checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
  }

  def byTag2 = {
    val fragments = Fragments(ff.tag("x"), text(" "), ex("e1"), ex("e2"))
    checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
  }

  def byTag3 = {
    val fragments = Fragments(ex("e1"), taggedAs("x"), ex("e2"))
    checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
  }

  def byTag4 = {
    val fragments = Fragments(ex("e1"), text(" "), taggedAs("x"), ex("e2"))
    checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
  }
  def byTag5 = {
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

  def byTag6 = {
    val fragments = Fragments(ex("e1"),
      ex("e2"), asSection("x"),
      ex("e3"),
      ex("e4"), asSection("x"),
      ex("e5")
    )
    checkSelection(fragments, "x", expected = Seq("e2", "e3", "e4"), unexpected = Seq("e1", "e5"))
  }

  def byTag7 = {
    val fragments = Fragments(
      asSection("x"),
      ff.text(" "),
      ex("e1"), asSection("x"),
      ex("e2")
    )
    checkSelection(fragments, "x", expected = Seq("e1"), unexpected = Seq("e2"))
  }

  def byTag8 = {
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
    checkSelection(fragments, Seq("x", "y"), expected = (2 to 8).map("e" + _),             unexpected = Seq("e1", "e9"))
  }

  def byTag9 = {
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

  def byTag10 = {
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
   filterIncluded(fragments, Seq("x")).map(_.description) ==== List(
      text("  "),
      ex("e2"),
      ff.break,
      ff.tab,
      ex("e4")
    ).map(_.description)
  }

  def byPrevious = {
    val repo = StatisticsRepositoryCreation.memory
    val arguments = Arguments.split("was x")
    val env = Env(arguments = arguments).setStatisticRepository(repo)
    repo.storeResult(getClass.getName, Text("e1"), org.specs2.execute.Failure("failed")).runOption
     val fragments = Fragments(
        ex("e1"),
        ex("e2")
       ).flatMap(f => eval[Action, Fragment](DefaultStatistics(arguments, repo).readStats(getClass.getName)(f).toAction))
   check(fragments, expected = Seq("e1"), unexpected = Seq("e2"))(env)
  }

  def check(fragments: Fragments, expected: Seq[String], unexpected: Seq[String])(env: Env): Result = {
    val executed = fragments |> DefaultSelector(env.arguments).filterByPrevious(env.arguments)
    val descriptions = executed.fragmentsList(ee).map(_.description.toString)
    expected.foreach(e => descriptions aka "expected for exclude" must contain(beMatching(".*"+e+".*")))
    unexpected.foreach(e => descriptions aka "unexpected for exclude"  must not(contain(beMatching(".*"+e+".*"))))
    ok
  }

  def support1 = {
    val original = Fragments(ex("e1"), ex("e2"), text(" "), taggedAs("t1"))
    val swapped = original |> DefaultSelector(Arguments()).swapBeforeMarkerAndEmptyText
    swapped.fragmentsList(ee).map(_.description.show) must_== List(ex("e1"), ex("e2"),
      ff.tag("t1"), text(" ")).map(_.description.show)
  }

  def support2 = {
    val original = Fragments(ex("e1"), ex("e2"), taggedAs("t1"))
    val swapped = original |> DefaultSelector(Arguments()).transformBeforeMarkersToAfterMarkers
    swapped.fragmentsList(ee).map(_.description.show) must_==
      List(ex("e1"), ff.tag("t1"), ex("e2")).map(_.description.show)
  }

  def support3 = {
    val original = Fragments(ex("e1"), tag("t1"), ex("e2"))
    val swapped = original |> DefaultSelector(Arguments()).transformTagsToSections
    swapped.fragmentsList(ee).map(_.description.show) must_==
      List(ex("e1"), ff.section("t1"), ex("e2"), ff.section("t1")).map(_.description.show)
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
    val descriptions = executed.fragmentsList(ee).map(_.description.show)

    s"${descriptions.mkString(",")} does not contain ${unexpected.mkString(",")} but contains ${expected.mkString(",")} for tags ${tags.mkString(",")}" ==> {
      Result.foreach(expected)  (e => descriptions aka "expected for exclude" must contain(beMatching(".*"+e+".*"))) and
      Result.foreach(unexpected)(e => descriptions aka "unexpected for exclude"  must not(contain(beMatching(".*"+e+".*"))))
    }
  }

  def filterIncluded(fragments: Fragments, tags: Seq[String]): List[Fragment] = {
    val arguments = Arguments.split(s"include ${tags.mkString(",")}")
    (fragments.contents |> DefaultSelector(arguments).filterByMarker(arguments)).runList.run(ee)
  }

  def filterExcluded(fragments: Fragments, tags: Seq[String]): Fragments = {
   val arguments = Arguments.split(s"exclude ${tags.mkString(",")}")
   fragments |> DefaultSelector(arguments).filterByMarker(arguments)
  }

  def show(fs: Fragments): String =
    fs.fragmentsList(ee).map(_.description).mkString("\n")
}
