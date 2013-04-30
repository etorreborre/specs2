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
  implicit def gwtStepsIsSpecPart(gwt: GWTSteps): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String) = {
      if (gwt.isStart) fs append section(gwt.title) append Text(text)
      else             fs append (gwt.fragments(text) append section(gwt.title))
    }
  }

  /**
   * start a sequence of GWT steps
   */
  object GWTSteps {
    def apply(title: String): GWTStart = GWTStart(title)
  }

  /**
   * A sequence of GWT steps.
   */
  trait GWTSteps {
    def title: String
    /** create fragments corresponding on this sequence based on a piece of text */
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

  case class GWTWhensApply[T, GT <: HList, GTE <: HList,
                              WT <: HList, WTR <: HList, WTE <: HList, WM <: HList](givens: GWTGivens[GT, GTE], whenExtractors: WTE, mappers: WM) {

    def apply[R](map: (T :: GT) => R) =
      GWTWhens[GT, GTE, WT, R :: WTR, WTE, ((T :: GT) => R) :: WM](givens, whenExtractors, map :: mappers)
  }

  case class GWTThens[GT <: HList, GTE <: HList,
                      WT <: HList, WTR <: HList, WTE <: HList, WM <: HList,
                      TTE <: HList, VE <: HList](whens: GWTWhens[GT, GTE, WT, WTR, WTE, WM],
                                                 thenExtractors: TTE, verifications: VE, isStart: Boolean = true) extends GWTSteps {

    def andThen[T](f: StepParser[T]) =
      GWTThensApply[T, GT, GTE, WT, WTR, WTE, WM, StepParser[T] :: TTE, VE](GWTWhens(whens.givens, whens.whenExtractors, whens.mappers), f :: thenExtractors, verifications)

    def title = whens.title
    def givens = whens.givens

    def fragments(text: String): Fragments = {
      val lines = text.split("\n").reverse.dropWhile(_.trim.isEmpty).reverse

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
  }

  case class GWTThensApply[T, GT <: HList, GTE <: HList,
                              WT <: HList, WTR <: HList, WTE <: HList, WM <: HList,
                              TTE <: HList, VE <: HList](whens: GWTWhens[GT, GTE, WT, WTR, WTE, WM],
                                                         thenExtractors: TTE, verifications: VE) {

    def apply[R : AsResult](verify: (T :: WTR) => R) =
      GWTThens[GT, GTE, WT, WTR, WTE, WM, TTE, ((T :: WTR) => Result) :: VE](whens, thenExtractors, AsResult.lift(verify) :: verifications)
  }

  private implicit def toListAny: ToList[HList, Any] = new ToList[HList, Any] {
    def apply(l: HList) = l match {
      case head :: HNil => List(head)
      case head :: tail => head :: tail.toList(toListAny)
      case _            => List[Any]()
    }
  }

  private def value(r: Result) = r match { case DecoratedResult(v, _) => v; case _ => r }

}
