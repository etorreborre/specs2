package org.specs2
package specification
package script

import shapeless.{ToList, HList, HNil, ::}
import execute._
import ResultLogicalCombinators._

/**
 * The GWT trait can be used to associate a piece of text to Given/When/Then steps according to the [BDD](http://en.wikipedia.org/wiki/Behavior-driven_development)
 * way of describing acceptance criteria
 */
trait GWT extends StepParsers with Scripts { outer: FragmentsBuilder =>
  /** renaming of the shapeless cons object to avoid imports */
  val :: = shapeless.::

  /**
   * start a sequence of GWT steps
   * by default the scenario template that is used is considering the last lines before the scenario end
   *  to form the given/when/then steps
   */
  object Scenario {
    def apply(title: String)(implicit template: ScriptTemplate[Scenario, GivenWhenThenLines] = LastLinesScriptTemplate()): GWTStart = GWTStart(title, template)
  }

  /**
   * Start of a scenario
   */
  case class GWTStart(title: String, template: ScriptTemplate[Scenario, GivenWhenThenLines], isStart: Boolean = true) extends Scenario {
    type S = GWTStart

    def given[T](f: StepParser[T]) = GWTGivens[T :: HNil, (StepParser[T]) :: HNil, T](title, template, f :: HNil)
    def fragments(text: String): Fragments = Fragments.createList(Text(text))

    def start: Scenario = this
    def end: Scenario = this
    def withTitle(t: String) = copy(title = t)

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
  case class GWTGivens[GT <: HList, GTE <: HList, GTU](title: String, template: ScriptTemplate[Scenario, GivenWhenThenLines], givenExtractors: GTE = HNil, isStart: Boolean = true) extends Scenario {
    type S = GWTGivens[GT, GTE, GTU]

    def given[T, U](f: StepParser[T])(implicit lub: ToList[T :: GT, U]) = GWTGivens[T :: GT, (StepParser[T]) :: GTE, U](title, template, f :: givenExtractors)
    def when[T](f: StepParser[T]) = GWTWhensApply[T, GT, GTE, GTU, T :: HNil, HNil, (StepParser[T]) :: HNil, HNil, Any](this, f :: HNil, HNil)

    def fragments(text: String): Fragments = Fragments.createList(Text(text))

    def start = copy(isStart = true)
    def end   = copy(isStart = false)
    def withTitle(t: String) = copy(title = t)

    def stepsNumbers = Seq(givenExtractors.toList.size)
  }

  case class GWTWhens[GT <: HList, GTE <: HList, GTU, WT <: HList, WTR <: HList, WTE <: HList, WM <: HList, WMU](givens: GWTGivens[GT, GTE, GTU], whenExtractors: WTE, mappers: WM, isStart: Boolean = true) extends Scenario {
    type S = GWTWhens[GT, GTE, GTU, WT, WTR, WTE, WM, WMU]

    def when[T](f: StepParser[T]) = GWTWhensApply[T, GT, GTE, GTU, T :: WT, WTR, (StepParser[T]) :: WTE, WM, WMU](givens, f :: whenExtractors, mappers)
    def andThen[T](f: StepParser[T]) = GWTThensApply[T, GT, GTE, GTU, WT, WTR, WTE, WM, WMU, (StepParser[T]) :: HNil, HNil](GWTWhens(givens, whenExtractors, mappers), f :: HNil, HNil)

    def title = givens.title
    def fragments(text: String): Fragments = Fragments.createList(Text(text))

    def start = copy(isStart = true)
    def end   = copy(isStart = false)
    def withTitle(t: String) = copy(givens = givens.withTitle(t))

    def stepsNumbers = givens.stepsNumbers :+ whenExtractors.toList.size
  }

  case class Mapper[T, P <: HList, U, R](f1: Option[(T :: P) => R] = None, f2: Option[(T, Seq[U]) => R] = None) {
    def apply(t: T, p: Either[P, Seq[U]]): R = if (f1.isDefined) f1.get(t :: p.left.get) else f2.get(t, p.right.get)
  }

  case class VerifyFunction[T, P <: HList, U, R : AsResult](f1: Option[(T :: P) => R] = None, f2: Option[(T, Seq[U]) => R] = None) {
    def apply(t: T, p: Either[P, Seq[U]]): Result = AsResult(if (f1.isDefined) f1.get(t :: p.left.get) else f2.get(t, p.right.get))
  }

  case class GWTWhensApply[T, GT <: HList, GTE <: HList, GTU,
                              WT <: HList, WTR <: HList, WTE <: HList, WM <: HList, WMU](givens: GWTGivens[GT, GTE, GTU], whenExtractors: WTE, mappers: WM) {

    def apply[R, U](map: (T :: GT) => R)(implicit lub: ToList[R :: WTR, U]) =
      GWTWhens[GT, GTE, GTU, WT, R :: WTR, WTE, Mapper[T, GT, Nothing, R] :: WM, U](givens, whenExtractors, Mapper[T, GT, Nothing, R](f1 = Some(map)) :: mappers)

    def collect[R, U](map: (T, Seq[GTU]) => R)(implicit lub: ToList[R :: WTR, U]) =
      GWTWhens[GT, GTE, GTU, WT, R :: WTR, WTE, Mapper[T, HNil, GTU, R] :: WM, U](givens, whenExtractors, Mapper[T, HNil, GTU, R](f2 = Some(map)) :: mappers)
  }

  case class GWTThens[GT <: HList, GTE <: HList, GTU,
                      WT <: HList, WTR <: HList, WTE <: HList, WM <: HList, WTU,
                      TTE <: HList, VE <: HList](whens: GWTWhens[GT, GTE, GTU, WT, WTR, WTE, WM, WTU],
                                                 thenExtractors: TTE, verifications: VE, isStart: Boolean = true) extends Scenario {
    type S = GWTThens[GT, GTE, GTU, WT, WTR, WTE, WM, WTU, TTE, VE]

    def andThen[T](f: StepParser[T]) =
      GWTThensApply[T, GT, GTE, GTU, WT, WTR, WTE, WM, WTU, StepParser[T] :: TTE, VE](GWTWhens(whens.givens, whens.whenExtractors, whens.mappers), f :: thenExtractors, verifications)

    def title = whens.title
    def givens = whens.givens
    def template = givens.template

    def fragments(text: String): Fragments = {
      val givenExtractorsList = givens.givenExtractors.toList.reverse
      val whenExtractorsList  = whens.whenExtractors.toList.reverse
      val thenExtractorsList  = thenExtractors.toList.reverse
      val verificationsList   = verifications.toList.reverse

      var givenStepsResults: HList = HNil
      var givenStepsResult: Result = Success()

      var whenStepsResults: HList = HNil
      var whenStepsResult: Result = Success()

      template.lines(text, this).lines.foldLeft(Fragments.createList()) { (fs, lines) =>
        lines match {
          case TextLines(ls) => fs append ls.map(l => Text(l+"\n"))
          case GivenLines(ls) => {
            val givenSteps: Seq[Step] = (givenExtractorsList zip ls).view.map { case (extractor, line) => Step(extractLine(extractor, line)) }
            val givenFragments: Seq[Fragment] = (givenExtractorsList zip ls zip givenSteps).
              map { case ((extractor: StepParser[_], l: String), s: Step) => Text(extractor.strip(l)+"\n") ^ s }.flatMap(_.middle)

            givenStepsResults = givenSteps.foldRight(HNil: HList) { (cur, res) => value(cur.execute) :: res }
            givenStepsResult = givenSteps.foldRight(Success(): Result) { (cur, res) => cur.execute and res }

            fs append givenFragments
          }
          case WhenLines(ls) => {
            val whenSteps = (whenExtractorsList zip ls zip whens.mappers.toList).map { case ((extractor, line), mapper) =>
              Step {
                if (givenStepsResult.isSuccess) {
                  extractLine(extractor, line) match {
                    case DecoratedResult(t, _) => {
                      val map = mapper.asInstanceOf[Mapper[Any, Any, Any, Any]]
                      if (map.f1.isDefined) map(t, Left(givenStepsResults))
                      else map(t, Right(givenStepsResults.toList))
                    }
                    case other                 => other
                  }
                } else Skipped(" ")
              }
            }

            val whenFragments: Seq[Fragment] = (whenExtractorsList zip ls zip whenSteps).
              map { case ((extractor: StepParser[_], l: String), s: Step) => Text(extractor.strip(l)+"\n"+l.takeWhile(_ == ' ').mkString) ^ s ^ Text("\n") }.flatMap(_.middle)

            whenStepsResults = whenSteps.foldRight(HNil: HList) { (cur, res) => value(cur.execute) :: res }
            whenStepsResult = whenSteps.foldRight(Success(): Result) { (cur, res) => cur.execute and res }

            fs append whenFragments
          }
          case ThenLines(ls) => {
            val thenExamples = (thenExtractorsList zip ls zip verificationsList).flatMap { case ((extractor: StepParser[_], line), verify) =>
              (Example(extractor.strip(line), {
                if (givenStepsResult.isSuccess && whenStepsResult.isSuccess) {
                  extractLine(extractor, line) match {
                    case DecoratedResult(t, _) => {
                      val verifyFunction = verify.asInstanceOf[VerifyFunction[Any, Any, Any, Any]]
                      if (verifyFunction.f1.isDefined) verifyFunction(t, Left(whenStepsResults)).asInstanceOf[Result]
                      else                             verifyFunction(t, Right(whenStepsResults.toList)).asInstanceOf[Result]
                    }
                    case other                 => other
                  }
                } else Skipped(" ")
              }) ^ Text("\n")).middle
            }
            fs append thenExamples
          }
        }
      }
    }

    private def extractLine(extractor: Any, line: String) =
      extractor.asInstanceOf[StepParser[Any]].parse(line).fold(e => Error(e), t => DecoratedResult(t, Success()))


    def start = copy(isStart = true)
    def end   = copy(isStart = false)
    def withTitle(t: String) = copy(whens = whens.withTitle(t))

    def stepsNumbers = whens.stepsNumbers :+ thenExtractors.toList.size
  }

  case class GWTThensApply[T, GT <: HList, GTE <: HList, GTU,
                              WT <: HList, WTR <: HList, WTE <: HList, WM <: HList, WMU,
                              TTE <: HList, VE <: HList](whens: GWTWhens[GT, GTE, GTU, WT, WTR, WTE, WM, WMU],
                                                         thenExtractors: TTE, verifications: VE) {

    def apply[R : AsResult](verify: (T :: WTR) => R) =
      GWTThens[GT, GTE, GTU, WT, WTR, WTE, WM, WMU, TTE, VerifyFunction[T, WTR, Nothing, R] :: VE](whens, thenExtractors, VerifyFunction[T, WTR, Nothing, R](f1 = Some(verify)) :: verifications)

    def collect[R : AsResult](verify: (T, Seq[WMU]) => R) =
      GWTThens[GT, GTE, GTU, WT, WTR, WTE, WM, WMU, TTE, VerifyFunction[T, HNil, WMU, R] :: VE](whens, thenExtractors, VerifyFunction[T, HNil, WMU, R](f2 = Some(verify)) :: verifications)

  }

  private implicit def toListAny[H <: HList]: ToList[H, Any] = new ToList[H, Any] {
    def apply(l: H) = l match {
      case head :: HNil => List(head)
      case head :: tail => head :: tail.toList(toListAny)
      case _            => List[Any]()
    }
  }

  private def value(r: Result) = r match { case DecoratedResult(v, _) => v; case _ => r }

  case class LastLinesScriptTemplate() extends ScriptTemplate[Scenario, GivenWhenThenLines] {
    def lines(text: String, script: Scenario) = {
      val ls = text.split("\n").reverse.dropWhile(_.trim.isEmpty).reverse

      val linesBlocks = Seq((ls: Seq[String]) => GivenLines(ls), (ls: Seq[String]) => WhenLines(ls), (ls: Seq[String]) => ThenLines(ls))
      val grouped = (script.stepsNumbers zip linesBlocks).reverse.foldLeft((GivenWhenThenLines(), ls)) { (res, cur) =>
        val (gwtLines, remainingLines) = res
        val (stepsNumber, linesBlock) = cur

        (gwtLines.prepend(linesBlock(remainingLines.takeRight(stepsNumber))), remainingLines.dropRight(stepsNumber))
      }
      // add the remaining lines at the beginning
      grouped._1.prepend(TextLines(grouped._2.toList))
    }
  }

  case class BulletTemplate(bullet: String = "*") extends ScriptTemplate[Scenario, GivenWhenThenLines] {
    def lines(text: String, script: Scenario): GivenWhenThenLines = {
      text.split("\n").foldLeft(GivenWhenThenLines()) { (res, line) =>
        val firstBulletWord =
          (if (line.trim.startsWith(bullet)) line.trim.drop(1).trim.split(" ").headOption.getOrElse("") else "").toLowerCase

        val newLine = line.replace(bullet+" ", "")
        if      (firstBulletWord.startsWith("given")) res.append(GivenLines(newLine))
        else if (firstBulletWord.startsWith("when"))  res.append(WhenLines(newLine))
        else if (firstBulletWord.startsWith("then"))  res.append(ThenLines(newLine))
        else res.append(TextLines(line))
      }
    }
  }

}

/**
 * A sequence of GWT steps.
 */
trait Scenario extends Script {
  type S <: Scenario
  def start: Scenario
  def end: Scenario

  def stepsNumbers: Seq[Int]
  def withTitle(t: String): S
}

/**
 * Set of extracted lines from some text which are either: simple text, given text, when text or then text
 */
case class GivenWhenThenLines(lines: Seq[GWTLines] = Seq()) extends ScriptLines {
  def prepend(ls: GWTLines) = (ls, lines.headOption) match {
    case (TextLines(l1), Some(TextLines(l2)))   => copy(lines = TextLines (l1 ++ l2) +: lines.drop(1))
    case (GivenLines(l1), Some(GivenLines(l2))) => copy(lines = GivenLines(l1 ++ l2) +: lines.drop(1))
    case (WhenLines(l1), Some(WhenLines(l2)))   => copy(lines = WhenLines (l1 ++ l2) +: lines.drop(1))
    case (ThenLines(l1), Some(ThenLines(l2)))   => copy(lines = ThenLines (l1 ++ l2) +: lines.drop(1))
    case _                                      => copy(lines = ls +: lines)
  }

  def append(ls: GWTLines) =
    (ls, lines.lastOption) match {
      case (TextLines(l1), Some(TextLines(l2)))   => copy(lines = lines.dropRight(1) :+ TextLines(l2 ++ l1))
      case (GivenLines(l1), Some(GivenLines(l2))) => copy(lines = lines.dropRight(1) :+ GivenLines(l2 ++ l1))
      case (WhenLines(l1), Some(WhenLines(l2)))   => copy(lines = lines.dropRight(1) :+ WhenLines(l2 ++ l1))
      case (ThenLines(l1), Some(ThenLines(l2)))   => copy(lines = lines.dropRight(1) :+ ThenLines(l2 ++ l1))
      case _                                      => copy(lines = lines :+ ls)
    }
}

trait GWTLines
case class GivenLines(lines: Seq[String]) extends GWTLines
object GivenLines { def apply(line: String): GivenLines = GivenLines(Seq(line)) }

case class WhenLines(lines: Seq[String]) extends GWTLines
object WhenLines { def apply(line: String): WhenLines = WhenLines(Seq(line)) }

case class ThenLines(lines: Seq[String]) extends GWTLines
object ThenLines { def apply(line: String): ThenLines = ThenLines(Seq(line)) }

case class TextLines(lines: Seq[String]) extends GWTLines
object TextLines { def apply(line: String): TextLines = TextLines(Seq(line)) }



