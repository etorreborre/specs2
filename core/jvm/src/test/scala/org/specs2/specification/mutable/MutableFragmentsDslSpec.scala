package org.specs2.specification.mutable

import org.specs2.concurrent.ExecutionEnv
import org.specs2.execute.Result
import org.specs2.main.ArgumentsShortcuts
import org.specs2.matcher.{Matcher, TraversableMatchers, TypedEqual}
import org.specs2.specification.core.{Fragment, SpecStructure, StacktraceLocation}
import org.specs2.specification.create.DefaultFragmentFactory._
import org.specs2.specification.dsl.mutable.{ArgumentsCreation, MutableDsl, MutableFragmentBuilder}
import org.specs2.matcher.MatchersImplicits._

class MutableFragmentsDslSpec(ee: ExecutionEnv) extends org.specs2.Spec with TypedEqual with TraversableMatchers { def is = s2"""

  create examples
    with a string and a result   $e1
    with a string and a for loop $e2

  create blocks
    with simple examples $e3
    with a for loop      $e4

  set a title on the specification $e5

  set arguments on the specification $e6
  set arguments twice on the specification $e7

  Breaks
    there must be 2 breaks after the specification title      $breaks1
    there must be 1 break after the "should" text of a block  $breaks2
    there must be 1 break after an example in a block         $breaks3

"""

  def e1 =
    fragments(new dsl { "e1" in ok }) must
      beTheSameFragments(break, break, example("e1", ok), break)

  def e2 = fragments(new dsl { "e1" in Result.foreach(1 to 2)(i => i === i) }) must
    beTheSameFragments(break, break, example("e1", ok), break)

  def e3 = fragments(new dsl {
    "this" should {
      "e1" in ok
      "e2" in ok
    }
  }).map(_.description) must
    contain(exactly(Seq(
      break,
      start, fragmentFactory.section("this should"), break,
      text("this should"), tab, break,
      example("e1", ok), break,
      example("e2", ok),
      break, backtab, fragmentFactory.section("this should"), end).map(_.description):_*))

  def e4 = fragments(new dsl {
    "this" should {
      Fragment.foreach(1 to 2) { i => "e"+i in ok }
    }
  }).map(_.description) must
    contain(exactly(Seq(
      break,
      start, fragmentFactory.section("this should"), break,
      text("this should"), tab, break,
      example("e1", ok), break,
      example("e2", ok),
      break, backtab, fragmentFactory.section("this should"), end).map(_.description):_*))

  def e5 = structure(new dsl {
    "have a title".title
    "this" should { "have an example" in ok }
  }).header.title must beSome("have a title")

  def e6 = structure(new dsl {
    args(plan = true)
    "this" should { "have an example" in ok }
  }).arguments.plan must beTrue

  def e7 = {
    val arguments = structure(new dsl with ArgumentsShortcuts with ArgumentsCreation {
      sequential
      isolated
      "this" should { "have an example" in ok }
    }).arguments

    (arguments.sequential must beTrue) and
    (arguments.isolated must beTrue)
  }

  def breaks1 = fragments(new dsl { "spec".title }).map(_.description) must
    contain(exactly(Seq(
      break, break).map(_.description):_*))

  def breaks2 = fragments(new dsl { "this" should { "be ok" in ok } }).map(_.description) must
    contain(allOf(Seq(
      start,
      text("this should"), tab, break).map(_.description):_*)).inOrder

  def breaks3 = fragments(new dsl { "this" should { "be ok" in ok } }).map(_.description) must
    contain(allOf(Seq(
      example("be ok", ok), break).map(_.description):_*)).inOrder

  def fragments(dsl1: dsl): List[Fragment] =
    structure(dsl1).fragmentsList(ee)

  def structure(dsl1: dsl): SpecStructure =
    dsl1.is

  trait dsl extends MutableFragmentBuilder with MutableDsl {
    addSections
  }

  def beTheSameFragments(fs: Fragment*): Matcher[Seq[Fragment]] = { actual: Seq[Fragment] =>
    val location = StacktraceLocation()
    actual.map(_.setLocation(location)) must contain(exactly(fs.map(_.setLocation(location)):_*))
  }
}
