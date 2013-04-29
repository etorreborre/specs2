package org.specs2
package specification

import execute._
import AsResult._
import shapeless._
import scalaz.std.list._
import scalaz.syntax.foldable._
import FragmentExecution._
import control.Exceptions._
import util.matching.Regex
import runner.TextRunner

class GivenWhenThenStepsSpec extends Specification with GivenWhenThenSteps with Grouped { def is = s2"""

 Given / When / Then is a style of specification where there are a number of steps setting up values to setup a context (given steps), then some steps to trigger some actions (when steps) and finally some checks (then steps).

 Combinations with delimited extractors

  given/when/then                                   ${g1.e1}
  given/given/when/then                             ${g1.e2}
  given/given/when/when/then                        ${g1.e3}
  given/when/then/then                              ${g1.e4}

 Extractors must extract values and return the resulting string

  with delimited extractors                         ${g2.e1}
  with regex extractors                             ${g2.e2}


   TODO:

 * test failures during: extraction, mapping, examples
 * test with RegexParsers
 * test with normal interpolated variables in the middle
 * remove the FragmentsParsers with variable stuff
 * document
 * provide default DelimitedStepParsers anInt, twoInts, threeInts, aString, twoStrings,
   threeStrings, aDouble, twoDoubles, threeDoubles and combination thereof (+ dates, sequences, times?)
 * use a template to define which lines must be mapped to extractors (how to skip some lines?)

 """

  "combinations" - new g1 {
    e1 := {
      val steps = GWTSteps("e1").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => a === e }

      executeExamplesResult {
        s2""" ${steps.start}
          given {1}
          when {-}
          then {-1}       ${steps.end}
        """
      }
    }

    e2 := {
      val steps = GWTSteps("e2").
        given(anInt).
        given(anInt).
        when(aString) { case op :: i :: j :: _ => i + j }.
        andThen(anInt) { case e :: a :: _ => a === e }

      executeExamplesResult {
        s2""" ${steps.start}
          given {1}
          given {2}
          when {+}
          then {3}       ${steps.end}
        """
      }
    }

    e3 := {
      val steps = GWTSteps("e3").
        given(anInt).
        given(anInt).
        when(aString) { case op :: i :: j :: _ => i + j }.
        when(aString) { case op :: _           => ((i:Int) => -i) }.
        andThen(anInt) { case e :: f :: a :: _ => f(a) === e }

      executeExamplesResult {
        s2""" ${steps.start}
          given {1}
          given {2}
          when {+}
          when {-}
          then {-3}       ${steps.end}
        """
      }
    }

    e4 := {
      val steps = GWTSteps("e4").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => a === e }.
        andThen(anInt) { case e :: a :: _ => a must be_>(e) }

      executeExamplesResult {
        s2""" ${steps.start}
          given {1}
          when {-}
          then {-1}
          then {-10}       ${steps.end}
        """
      }
    }
  }

  "extractors" - new g2 {
    e1 := {
      val steps = GWTSteps("e1").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => a === e }

      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {-}
          then {-1}       ${steps.end}
        """
      }.replace("\n", "") must contain("given 1") and contain("when -") and contain("+ then -1")
    }

    e2 := {
      val anInt = groupAs("\\-?\\d+").and((_:String).toInt)
      val lastString = groupAs("\\w+").and((ss: Seq[String]) => ss.last)

      val steps = GWTSteps("e2").
        given(anInt).
        when(lastString) { case op :: i :: _ => -i }.
        andThen(anInt)   { case e :: a :: _ => a === e }

      toText { nocolor ^
        s2""" ${steps.start}
          given 1
          when -
          then -1       ${steps.end}
        """
      }.replace("\n", "") must contain("given 1") and contain("when -") and contain("+ then -1")

    }
  }
  
  def toText(fs: Fragments) = (new TextRunner)(fs)
}

trait GivenWhenThenSteps extends GWT with Tags { this: Specification =>

  val addition = GWTSteps("addition").
    given(anInt).
    given(anInt).
    when(aString) { case operator :: a :: b:: HNil => a + b }.
    andThen(anInt) { case expected :: sum :: HNil => sum === expected }

  def anInt = StepParser((_:String).toInt)
  def aString = StepParser((s:String) => s)


}
