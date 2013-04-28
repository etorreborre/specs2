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
 * provide default DelimitedFragmentParsers anInt, twoInts, threeInts, aString, twoStrings,
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

trait GivenWhenThenSteps extends FragmentParsers with Tags { this: Specification =>

  val addition = GWTSteps("addition").
    given(anInt).
    given(anInt).
    when(aString) { case operator :: a :: b:: HNil => a + b }.
    andThen(anInt) { case expected :: sum :: HNil => sum === expected }

  def anInt = FragmentParser((_:String).toInt)
  def aString = FragmentParser((s:String) => s)

  implicit def gwtStepsIsSpecPart(gwt: GWTSteps): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String) = {
      if (gwt.isStart) fs append section(gwt.title) append Text(text)
      else             fs append (gwt.fragments(text) append section(gwt.title))
    }
  }

  object GWTSteps {
    def apply(title: String): GWTStart = GWTStart(title)
  }

  trait GWTSteps {
    def title: String
    def fragments(text: String): Fragments
    def isStart: Boolean
    def start: GWTSteps
    def end: GWTSteps
  }

  case class GWTStart(title: String, isStart: Boolean = true) extends GWTSteps {
    def given[T](f: StepParser[T]) = GWTGivens[T :: HNil, (StepParser[T]) :: HNil](title, f :: HNil)
    def fragments(text: String): Fragments = Fragments.createList(Text(text))
    def start: GWTSteps = this
    def end: GWTSteps = this
  }

  /**
   * GT = Given types, types of extracted values for then steps
   * GTE = Given extractor types, types of extractor functions
   * WT = When types, types of extracted values for when steps
   * WTR = When result types, after mapping
   * WTE = When extractor types
   * WM = Mapping functions types
   * TTE = Then types extractors
   * VE =  verification types for then steps
   */
  case class GWTGivens[GT <: HList, GTE <: HList](title: String, givenExtractors: GTE = HNil, isStart: Boolean = true) extends GWTSteps {
    def given[T](f: StepParser[T]) = GWTGivens[T :: GT, (StepParser[T]) :: GTE](title, f :: givenExtractors)
    def when[T](f: StepParser[T]) = GWTWhensApply[T, GT, GTE, T :: HNil, HNil, (StepParser[T]) :: HNil, HNil](this, f :: HNil, HNil)

    def fragments(text: String): Fragments = Fragments.createList(Text(text))

    def start = copy(isStart = true)
    def end   = copy(isStart = false)
  }

  case class GWTWhens[GT <: HList, GTE <: HList, WT <: HList, WTR <: HList, WTE <: HList, WM <: HList](givens: GWTGivens[GT, GTE], whenExtractors: WTE, mappers: WM, isStart: Boolean = true) extends GWTSteps {
    def when[T](f: StepParser[T])    = GWTWhensApply[T, GT, GTE, T :: WT, WTR, (StepParser[T]) :: WTE, WM](givens, f :: whenExtractors, mappers)
    def andThen[T](f: StepParser[T]) = GWTThensApply[T, GT, GTE, WT, WTR, WTE, WM, (StepParser[T]) :: HNil, HNil](GWTWhens(givens, whenExtractors, mappers), f :: HNil, HNil)

    def title = givens.title
    def fragments(text: String): Fragments = Fragments.createList(Text(text))

    def start = copy(isStart = true)
    def end   = copy(isStart = false)
  }

  case class GWTWhensApply[T, GT <: HList, GTE <: HList, WT <: HList, WTR <: HList, WTE <: HList, WM <: HList](givens: GWTGivens[GT, GTE], whenExtractors: WTE, mappers: WM) {
    def apply[R](map: (T :: GT) => R) =
      GWTWhens[GT, GTE, WT, R :: WTR, WTE, ((T :: GT) => R) :: WM](givens, whenExtractors, map :: mappers)
  }

  case class GWTThens[GT <: HList, GTE <: HList, WT <: HList, WTR <: HList, WTE <: HList, WM <: HList, TTE <: HList, VE <: HList](
      whens: GWTWhens[GT, GTE, WT, WTR, WTE, WM],
      thenExtractors: TTE, verifications: VE, isStart: Boolean = true) extends GWTSteps {

    def andThen[T](f: StepParser[T]) =
      GWTThensApply[T, GT, GTE, WT, WTR, WTE, WM, StepParser[T] :: TTE, VE](GWTWhens(whens.givens, whens.whenExtractors, whens.mappers), f :: thenExtractors, verifications)

    def title = whens.title
    def givens = whens.givens

    def fragments(text: String): Fragments = {
      val lines = text.split("\n").reverse.dropWhile(_.trim.isEmpty).reverse

      implicit def toListAny: ToList[HList, Any] = new ToList[HList, Any] {
        def apply(l: HList) = l match {
          case head :: HNil => List(head)
          case head :: tail => head :: tail.toList(toListAny)
          case _            => List[Any]()
        }
      }
      def value(r: Result) = r match { case DecoratedResult(v, _) => v; case _ => r }

      val givenExtractorsList = givens.givenExtractors.toList(toListAny).reverse
      val whenExtractorsList  = whens.whenExtractors.toList(toListAny).reverse
      val thenExtractorsList  = thenExtractors.toList(toListAny).reverse
      val verificationsList   = verifications.toList(toListAny).reverse

      val (intro, givenLines, whenLines, thenLines) =
        (lines.dropRight(whenExtractorsList.size + thenExtractorsList.size + givenExtractorsList.size),
         lines.dropRight(whenExtractorsList.size + thenExtractorsList.size).takeRight(givenExtractorsList.size),
         lines.dropRight(thenExtractorsList.size).takeRight(whenExtractorsList.size),
         lines.takeRight(thenExtractorsList.size))

      val introFragments: Seq[Fragment] = intro.map(l => Text(l+"\n"))
      def extractLine(extractor: Any, line: String) =
        extractor.asInstanceOf[StepParser[Any]].parse(line).fold(s => org.specs2.execute.Error(s), t => DecoratedResult(t, org.specs2.execute.Success()))

      val givenSteps: Seq[Step] = (givenExtractorsList zip givenLines).view.map { case (extractor, line) => Step(extractLine(extractor, line)) }
      val givenFragments: Seq[Fragment] = (givenExtractorsList zip givenLines zip givenSteps).
        map { case ((extractor: StepParser[_], l: String), s: Step) => Text(extractor.strip(l)+"\n") ^ s }.flatMap(_.middle)

      lazy val givenStepsResults = givenSteps.foldRight(HNil: HList) { (cur, res) => value(cur.execute) :: res }
      lazy val givenStepsResult = givenSteps.foldRight(org.specs2.execute.Success(): Result) { (cur, res) => cur.execute and res }
      val whenSteps =  (whenExtractorsList zip whenLines zip whens.mappers.toList(toListAny)).map { case ((extractor, line), mapper) =>
        Step {
          if (givenStepsResult.isSuccess) {
            val extracted = extractLine(extractor, line)
            val values = extracted :: givenStepsResults
            mapper.asInstanceOf[Any => Any](values)
          } else Skipped("Given steps are failing")
        }
      }

      val whenFragments: Seq[Fragment] = (whenExtractorsList zip whenLines zip whenSteps).
        map { case ((extractor: StepParser[_], l: String), s: Step) => Text(extractor.strip(l)+"\n") ^ s }.flatMap(_.middle)

      lazy val whenStepsResults = whenSteps.foldRight(HNil: HList) { (cur, res) => value(cur.execute) :: res }
      lazy val whenStepsResult = whenSteps.foldRight(org.specs2.execute.Success(): Result) { (cur, res) => cur.execute and res }

      val thenExamples = (thenExtractorsList zip thenLines zip verificationsList).map { case ((extractor: StepParser[_], line), verify) =>
        Example(extractor.strip(line),
        {
          if (givenStepsResult.isSuccess && whenStepsResult.isSuccess) {
            extractLine(extractor, line) match {
              case DecoratedResult(t, _) => verify.asInstanceOf[Any => Any](t :: whenStepsResults).asInstanceOf[Result]
              case other                 => other
            }
          } else Skipped("Previous steps are failing")
        })
      }

      Fragments.createList((introFragments ++ givenFragments ++ whenFragments ++ thenExamples) :_*)
    }

    def start = copy(isStart = true)
    def end   = copy(isStart = false)
  }

  case class GWTThensApply[T, GT <: HList, GTE <: HList, WT <: HList, WTR <: HList, WTE <: HList, WM <: HList, TTE <: HList, VE <: HList](
    whens: GWTWhens[GT, GTE, WT, WTR, WTE, WM],
    thenExtractors: TTE, verifications: VE) {

    def apply[R : AsResult](verify: (T :: WTR) => R) =
      GWTThens[GT, GTE, WT, WTR, WTE, WM, TTE, ((T :: WTR) => Result) :: VE](whens, thenExtractors, AsResult.lift(verify) :: verifications)
  }

  /** factory method to create a Given or a Then element from a regex */
  def readAs(regex: String) = new ReadAs(regex.r)
  /** factory method to create a Given or a Then element from a regex, using a regex denoting groups to extract */
  def groupAs(groupRegex: String) = new ReadAs(groups = s"($groupRegex)".r)

  import RegexStep._

  /** This class creates Given or Then extractors from a regular expression and a function */
  class ReadAs(regex: Regex = "".r, groups: Regex = """\{([^}]+)\}""".r) {
    def apply(f: String => Unit) = and[Unit](f)

    def apply(f: (String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: (String, String, String, String, String, String, String, String, String, String) => Unit) = and[Unit](f)
    def apply(f: Seq[String] => Unit)(implicit p: ImplicitParam) = and[Unit](f)(p,p)

    private def value[T](t: =>T) = trye(t)(_.getMessage)

    def and[T](f: String => T) = new StepParser[T] {
      def parse(text: String) = value(f(extract1(text, regex, groups)))
    }
    def and[T](f: (String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract2(text, regex, groups)))
    }
    def and[T](f: (String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract3(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract4(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract5(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract6(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract7(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract8(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract9(text, regex, groups)))
    }
    def and[T](f: (String, String, String, String, String, String, String, String, String, String) => T) = new StepParser[T] {
      def parse(text: String) = value(f.tupled(extract10(text, regex, groups)))
    }
    def and[T](f: Seq[String] => T)(implicit p1: ImplicitParam, p2: ImplicitParam) = new StepParser[T] {
      def parse(text: String)  = value(f(extractAll(text, regex, groups)))
    }
  }
}
