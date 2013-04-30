package org.specs2
package specification

import util.matching._
import control.Exceptions._
import shapeless.{ToList, HList, HNil, ::}
import execute.{AsResult, Skipped, DecoratedResult, Result}

/**
 * The GWT trait can be used to associate a piece of text to Given/When/Then steps according to the [BDD](http://en.wikipedia.org/wiki/Behavior-driven_development)
 * way of describing acceptance criteria
 */
trait GWT extends StepParsers with Tags { outer: Specification =>
  /**
   * a sequence of GWT steps can be inserted in a specification to delimit
   * pieces of text to interpret. The "given/when" steps create execute.Step objects while the "then" steps create Examples
   *
   * The whole sequence also creates one tagged section with the title of the sequence
   */
  implicit def gwtStepsIsSpecPart(gwt: Scenario): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String) = {
      if (gwt.isStart) fs append section(gwt.title) append Text(text)
      else             fs append (gwt.fragments(text) append section(gwt.title))
    }
  }

  /**
   * start a sequence of GWT steps
   */
  object Scenario {
    def apply(title: String): GWTStart = GWTStart(title)
  }

  /**
   * A sequence of GWT steps.
   */
  trait Scenario extends Script {
    def title: String
    /** create fragments corresponding on this sequence based on a piece of text */
    def fragments(text: String): Fragments
    def isStart: Boolean
    def start: Scenario
    def end: Scenario

    def stepsNumbers: Seq[Int]
  }

  case class GWTStart(title: String, template: LastLinesScriptTemplate = LastLinesScriptTemplate(), isStart: Boolean = true) extends Scenario {
    def given[T](f: StepParser[T]) = GWTGivens[T :: HNil, (StepParser[T]) :: HNil](title, template, f :: HNil)
    def fragments(text: String): Fragments = Fragments.createList(Text(text))
    def start: Scenario = this
    def end: Scenario = this
    def stepsNumbers = Seq()
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
  case class GWTGivens[GT <: HList, GTE <: HList](title: String, template: LastLinesScriptTemplate = LastLinesScriptTemplate(), givenExtractors: GTE = HNil, isStart: Boolean = true) extends Scenario {
    def given[T](f: StepParser[T]) = GWTGivens[T :: GT, (StepParser[T]) :: GTE](title, template, f :: givenExtractors)
    def when[T](f: StepParser[T]) = GWTWhensApply[T, GT, GTE, T :: HNil, HNil, (StepParser[T]) :: HNil, HNil](this, f :: HNil, HNil)

    def fragments(text: String): Fragments = Fragments.createList(Text(text))

    def start = copy(isStart = true)
    def end   = copy(isStart = false)
    def stepsNumbers = Seq(givenExtractors.toList.size)
  }

  case class GWTWhens[GT <: HList, GTE <: HList, WT <: HList, WTR <: HList, WTE <: HList, WM <: HList](givens: GWTGivens[GT, GTE], whenExtractors: WTE, mappers: WM, isStart: Boolean = true) extends Scenario {
    def when[T](f: StepParser[T])    = GWTWhensApply[T, GT, GTE, T :: WT, WTR, (StepParser[T]) :: WTE, WM](givens, f :: whenExtractors, mappers)
    def andThen[T](f: StepParser[T]) = GWTThensApply[T, GT, GTE, WT, WTR, WTE, WM, (StepParser[T]) :: HNil, HNil](GWTWhens(givens, whenExtractors, mappers), f :: HNil, HNil)

    def title = givens.title
    def fragments(text: String): Fragments = Fragments.createList(Text(text))

    def start = copy(isStart = true)
    def end   = copy(isStart = false)
    def stepsNumbers = givens.stepsNumbers :+ whenExtractors.toList.size
  }

  case class GWTWhensApply[T, GT <: HList, GTE <: HList,
                              WT <: HList, WTR <: HList, WTE <: HList, WM <: HList](givens: GWTGivens[GT, GTE], whenExtractors: WTE, mappers: WM) {

    def apply[R](map: (T :: GT) => R) =
      GWTWhens[GT, GTE, WT, R :: WTR, WTE, ((T :: GT) => R) :: WM](givens, whenExtractors, map :: mappers)
  }

  case class GWTThens[GT <: HList, GTE <: HList,
                      WT <: HList, WTR <: HList, WTE <: HList, WM <: HList,
                      TTE <: HList, VE <: HList](whens: GWTWhens[GT, GTE, WT, WTR, WTE, WM],
                                                 thenExtractors: TTE, verifications: VE, isStart: Boolean = true) extends Scenario {

    def andThen[T](f: StepParser[T]) =
      GWTThensApply[T, GT, GTE, WT, WTR, WTE, WM, StepParser[T] :: TTE, VE](GWTWhens(whens.givens, whens.whenExtractors, whens.mappers), f :: thenExtractors, verifications)

    def title = whens.title
    def givens = whens.givens
    def template = givens.template

    def fragments(text: String): Fragments = {
      val givenExtractorsList = givens.givenExtractors.toList.reverse
      val whenExtractorsList  = whens.whenExtractors.toList.reverse
      val thenExtractorsList  = thenExtractors.toList.reverse
      val verificationsList   = verifications.toList.reverse

      val groupedLines = template.lines(text, this).lines
      val (intro, givenLines, whenLines, thenLines) = (groupedLines(0).lines, groupedLines(1).lines, groupedLines(2).lines, groupedLines(3).lines)

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
            extractLine(extractor, line) match {
              case DecoratedResult(t, _) => mapper.asInstanceOf[Any => Any](t :: givenStepsResults)
              case other                 => other
            }
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
    def stepsNumbers = whens.stepsNumbers :+ thenExtractors.toList.size
  }

  case class GWTThensApply[T, GT <: HList, GTE <: HList,
                              WT <: HList, WTR <: HList, WTE <: HList, WM <: HList,
                              TTE <: HList, VE <: HList](whens: GWTWhens[GT, GTE, WT, WTR, WTE, WM],
                                                         thenExtractors: TTE, verifications: VE) {

    def apply[R : AsResult](verify: (T :: WTR) => R) =
      GWTThens[GT, GTE, WT, WTR, WTE, WM, TTE, ((T :: WTR) => Result) :: VE](whens, thenExtractors, AsResult.lift(verify) :: verifications)
  }

  private implicit def toListAny[H <: HList]: ToList[H, Any] = new ToList[H, Any] {
    def apply(l: H) = l match {
      case head :: HNil => List(head)
      case head :: tail => head :: tail.toList(toListAny)
      case _            => List[Any]()
    }
  }

  private def value(r: Result) = r match { case DecoratedResult(v, _) => v; case _ => r }

  trait Script
  trait ScriptLines {
    def lines: Seq[Lines]
  }
  case class GivenWhenThenLines(lines: Seq[Lines] = Seq()) extends ScriptLines {
    def prepend(ls: Seq[String]) = copy(lines = Lines(ls) +: lines)
  }

  case class Lines(lines: Seq[String])

  trait ScriptTemplate[T <: Script, L <: ScriptLines] {
    def lines(text: String, script: T): L
  }

  case class LastLinesScriptTemplate() extends ScriptTemplate[Scenario, GivenWhenThenLines] {
    def lines(text: String, script: Scenario) = {
      val ls = text.split("\n").reverse.dropWhile(_.trim.isEmpty).reverse

      val grouped = script.stepsNumbers.reverse.foldLeft((GivenWhenThenLines(), ls)) { (res, cur) =>
        val (gwtLines, remainingLines) = res
        (gwtLines.prepend(remainingLines.takeRight(cur)), remainingLines.dropRight(cur))
      }
      // add the remaining lines at the beginning
      grouped._1.prepend(grouped._2)
    }
  }
}


