package org.specs2
package specification

import execute._
import script._
import shapeless._
import FragmentExecution._
import runner.TextRunner

class GWTSpec extends Specification with GWT with Grouped with StandardDelimitedStepParsers { def is = s2"""

 Given / When / Then is a style of specification where there are a number of steps setting up values to setup a context (given steps), then some steps to trigger some actions (when steps) and finally some checks (then steps).

 Combinations with delimited extractors

  given/when/then                                                ${g1.e1}
  given/given/when/then                                          ${g1.e2}
  given/given/when/when/then                                     ${g1.e3}
  given/when/then/then                                           ${g1.e4}

 Extractors must extract values and return the resulting string

  with delimited extractors                                      ${g2.e1}
  with regex extractors                                          ${g2.e2}

 If there are errors, the rest of the sequence must be skipped,
 but if there is a failure in a then step, the other steps must still be executed

  in a given step                                                ${g3.e1}
  in a when step - extraction error                              ${g3.e2}
  in a when step - mapping error                                 ${g3.e3}
  in a then step - extraction error                              ${g3.e4}
  in a then step - verification error                            ${g3.e5}
  in a then step - verification failure                          ${g3.e6}
  in a then step - verification failure in 2nd step              ${g3.e7}

 It is possible to intercalate other variables in the gwt steps
  a simple variable                                              ${g4.e1}

 Templates can be used to define which lines should be used      ${g5.e1}

   TODO:

 * document
 * use a template to define which lines must be mapped to extractors (how to skip some lines?)

 """

  "combinations" - new g1 {
    e1 := {
      val steps = Scenario("e1").
        given(groupAs("\\d").and((_:String).toInt)).
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
      val steps = Scenario("e2").
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
      val steps = Scenario("e3").
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
      val steps = Scenario("e4").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => a === e }.
        andThen(anInt) { case e :: a :: _ => a must be_>(e) }

      executeExamplesResult {
        s2"""              ${steps.start}
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
      val steps = Scenario("e1").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => a === e }

      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {-}
          then {-1}       ${steps.end}
        """
      } must contain("given 1") and contain("when -") and contain("+ then -1")
    }

    e2 := {
      val anInt = groupAs("\\-?\\d+").and((_:String).toInt)
      val lastString = groupAs("\\w+").and((ss: Seq[String]) => ss.last)

      val steps = Scenario("e2").
        given(anInt).
        when(lastString) { case op :: i :: _ => -i }.
        andThen(anInt)   { case e :: a :: _ => a === e }

      toText { nocolor ^
        s2""" ${steps.start}
          given 1
          when -
          then -1       ${steps.end}
        """
      } must contain("given 1") and contain("when -") and contain("+ then -1")

    }
  }

  "errors" - new g3 {
    e1 := {
      val steps = Scenario("e1").
        given(twoInts).
        when(aString) { case op :: (i, j) :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => a === e }

      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {-}
          then {-1}       ${steps.end}
        """
      } must contain("given 1! step error") and contain("when -o skipped") and contain("o then")
    }

    e2 := {
      val steps = Scenario("e2").
        given(anInt).
        when(twoStrings) { case (o, p) :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => a === e }

      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {-}
          then {-1}       ${steps.end}
        """
      } must contain("given 1") and contain("when -! step error") and contain("o then")
    }

    e3 := {
      val steps = Scenario("e3").
        given(anInt).
        when(aString) { case op :: i :: _ => op.toInt; -i }.
        andThen(anInt) { case e :: a :: _ => a === e }

      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {-}
          then {-1}       ${steps.end}
        """
      } must contain("given 1") and contain("when -! step error") and contain("o then")
    }

    e4 := {
      val steps = Scenario("e4").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(twoInts) { case e :: a :: _ => a === e }

      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {-}
          then {-1}       ${steps.end}
        """
      } must contain("given 1") and contain("when -") and contain("! then")
    }

    e5 := {
      val steps = Scenario("e5").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => "".toInt; a === e }

      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {-}
          then {-1}       ${steps.end}
        """
      } must contain("given 1") and contain("when -") and contain("! then")
    }

    e6 := {
      val steps = Scenario("e6").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => (a + 1) === e }

      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {-}
          then {-1}       ${steps.end}
        """
      } must contain("given 1") and contain("when -") and contain("x then")
    }

    e7 := {
      val steps = Scenario("e7").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => (a + 1) === e }.
        andThen(anInt) { case e :: a :: _ => a === e }

      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {-}
          then {-1}
          then {-1}       ${steps.end}
        """
      } must contain("given 1") and contain("when -") and contain("x then") and contain ("+ then")
    }
  }

  "other variables" - new g4 {
    e1 := {
      val steps = Scenario("e1").
        given(anInt).
        when(aString) { case op :: i :: _ => -i }.
        andThen(anInt) { case e :: a :: _ => a === e }

      val sign = "-"
      toText { nocolor ^
        s2"""             ${steps.start}
          given {1}
          when {$sign}
          then {-1}       ${steps.end}
        """
      } must contain("+ then")
    }
  }

  "templates" - new g5 with StandardRegexStepParsers {
    e1 := {
      implicit val bulletTemplate = BulletTemplate
      val steps = Scenario("e1").
        given(anInt).
        when(anInt)    { case i :: j :: _ => i + j }.
        andThen(anInt) { case e :: a :: _ => a === e }


      toText { nocolor ^
        s2"""                ${steps.start}
          These are the steps for an addition
           * given 1
           * when 2
           * then 3          ${steps.end}
        """
      } must contain("+ then")
    }
  }

  def toText(fs: Fragments) = (new TextRunner)(fs).replace("\n", "")

  val addition = Scenario("addition").
    given(anInt).
    given(anInt).
    when(aString) { case operator :: a :: b:: HNil => a + b }.
    andThen(anInt) { case expected :: sum :: HNil => sum === expected }

  case class BulletTemplate(bullet: String = "*") extends ScriptTemplate[Scenario, GivenWhenThenLines] {
    def lines(text: String, script: Scenario): GivenWhenThenLines = {
      text.split("\n").foldLeft(GivenWhenThenLines()) { (res, line) =>
        val firstBulletWord = if (line.trim.startsWith(bullet)) line.trim.drop(1).trim.split(" ").headOption.getOrElse("") else ""
        if (firstBulletWord.toLowerCase.startsWith("given")) res.append(GivenLines(line))
        else if (firstBulletWord.toLowerCase.startsWith("when")) res.append(WhenLines(line))
        else if (firstBulletWord.toLowerCase.startsWith("then")) res.append(ThenLines(line))
        else res.append(TextLines(line))
      }
    }
  }

}
