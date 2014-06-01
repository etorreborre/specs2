package org.specs2
package specification
package script

import org.specs2.specification.core.{Fragment, Fragments}
import specification.create.FragmentsFactory
import shapeless.{HList, HNil, ::}
import shapeless.ops.hlist.ToList
import execute._
import ResultLogicalCombinators._
import scalaz.std.list._
import scalaz.syntax.std.list._

/**
 * The GWT trait can be used to associate a piece of text to Given/When/Then steps according to the [BDD](http://en.wikipedia.org/wiki/Behavior-driven_development)
 * way of describing acceptance criteria
 */
trait GWT extends StepParsers with Scripts { outer: FragmentsFactory =>
  private val factory = fragmentFactory; import factory._

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

  private lazy val allAsString = new StepParser[String] {
    def parse(text: String) = Right(text)
    override def strip(text: String) = text
  }


  /**
   * Start of a scenario
   */
  case class GWTStart(title: String, template: ScriptTemplate[Scenario, GivenWhenThenLines], isStart: Boolean = true) extends Scenario {
    type S = GWTStart

    def given[T](f: StepParser[T]) = GWTGivens[T :: HNil, (StepParser[T]) :: HNil, T](title, template, f :: HNil)
    def given(): GWTGivens[String :: HNil, (StepParser[String]) :: HNil, String] = given(allAsString)
    def when() = GWTGivens[HNil, HNil, Unit](title, template, HNil).when()
    def when[R, U](value: =>R)(implicit lub: ToList[R :: HNil, U]) = GWTGivens[HNil, HNil, Unit](title, template, HNil).when().apply[R, U] { case _ => value }

    def fragments(text: String): Fragments = Fragments(fragmentFactory.Text(text))

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
    def given[U]()(implicit lub: ToList[String :: GT, U]): GWTGivens[String :: GT, (StepParser[String]) :: GTE, U] = given(allAsString)
    def when[T](f: StepParser[T]) = GWTWhensApply[T, GT, GTE, GTU, T :: HNil, HNil, (StepParser[T]) :: HNil, HNil, Any](this, f :: HNil, HNil)
    def when(): GWTWhensApply[String, GT, GTE, GTU, String :: HNil, HNil, (StepParser[String]) :: HNil, HNil, Any] = when(allAsString)

    def fragments(text: String): Fragments = Fragments(fragmentFactory.Text(text))

    def start = copy(isStart = true)
    def end   = copy(isStart = false)
    def withTitle(t: String) = copy(title = t)

    def stepsNumbers = Seq(givenExtractors.toList.size)
  }

  case class GWTWhens[GT <: HList, GTE <: HList, GTU, WT <: HList, WTR <: HList, WTE <: HList, WM <: HList, WMU](givens: GWTGivens[GT, GTE, GTU], whenExtractors: WTE, mappers: WM, isStart: Boolean = true) extends Scenario {
    type S = GWTWhens[GT, GTE, GTU, WT, WTR, WTE, WM, WMU]

    def when[T](f: StepParser[T]) = GWTWhensApply[T, GT, GTE, GTU, T :: WT, WTR, (StepParser[T]) :: WTE, WM, WMU](givens, f :: whenExtractors, mappers)
    def when(): GWTWhensApply[String, GT, GTE, GTU, String :: WT, WTR, (StepParser[String]) :: WTE, WM, WMU] = when(allAsString)
    def when[R, U](value: =>R)(implicit lub: ToList[R :: WTR, U]): GWTWhens[GT, GTE, GTU, String :: WT, R :: WTR, (StepParser[String]) :: WTE, Mapper[String,GT,Nothing,R] :: WM, U]  =
      when().apply[R, U] { case _ => value }

    def andThen[T](f: StepParser[T]) = GWTThensApply[T, GT, GTE, GTU, WT, WTR, WTE, WM, WMU, (StepParser[T]) :: HNil, HNil](GWTWhens(givens, whenExtractors, mappers), f :: HNil, HNil)
    def andThen(): GWTThensApply[String, GT, GTE, GTU, WT, WTR, WTE, WM, WMU, (StepParser[String]) :: HNil, HNil] = andThen(allAsString)

    def title = givens.title
    def fragments(text: String): Fragments = Fragments(fragmentFactory.Text(text))

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

    def andThen(): GWTThensApply[String, GT, GTE, GTU, WT, WTR, WTE, WM, WTU, StepParser[String] :: TTE, VE] = andThen(allAsString)

    def title = whens.title
    def givens = whens.givens
    def template = givens.template

    /**
     * use the template to parse the text and return blocks of TextLines, GivenLines, WhenLines, ThenLines
     *
     * For each of this blocks create steps and examples with execution dependencies (values created in a steps and used in
     * another one).
     */
    def fragments(text: String): Fragments = {

      /** results of given and when steps, stored in the reverse order of definition */
      var givenSteps: Seq[Fragment] = Seq()
      var whenSteps: Seq[Fragment]  = Seq()

      template.lines(text, this).lines.foldLeft(Fragments()) { (fs, lines) =>
        lines match {
          // Text lines are left untouched
          case TextLines(ls) => fs append fragmentFactory.Text(ls+"\n")

          // Given lines must create steps with extracted values
          case GivenLines(ls) => {
            givenSteps = (givenExtractorsList zip ls).map { case (extractor, line) => fragmentFactory.Step(extractLine(extractor, line)) }.reverse
            fs append appendSteps(givenExtractorsList, ls, givenSteps)
          }

          // When lines must create steps with extracted values, and map them using given values
          case WhenLines(ls) => {
            whenSteps = (whenExtractorsList zip ls zip whenMappersList).map { case ((extractor, line), mapper) =>
              Step(execute(result(givenSteps), extractor, line) { t: Any =>
                val map = mapper.asInstanceOf[Mapper[Any, HList, Any, Any]]
                map(t, if (map.f1.isDefined) Left(stepsValues(givenSteps)) else Right(stepsValues(givenSteps).toList))
              })
            }.reverse

            fs append appendSteps(whenExtractorsList, ls, whenSteps)
          }

          // Then lines must create examples with previous when values
          case ThenLines(ls) => {
            val thenExamples: List[Fragment] = (thenExtractorsList zip ls zip verificationsList).map { case ((extractor: StepParser[_], line), verify) =>
              Example(extractor.strip(line),
                execute(result(givenSteps) and result(whenSteps), extractor, line) { t: Any =>
                  val verifyFunction = verify.asInstanceOf[VerifyFunction[Any, HList, Any, Any]]
                  verifyFunction(t, if (verifyFunction.f1.isDefined) Left(stepsValues(whenSteps)) else Right(stepsValues(whenSteps).toList))
                }.asInstanceOf[Result])
            }
            fs append thenExamples.intersperse(fragmentFactory.Text("\n"))
          }
        }
      }
    }


    def start = copy(isStart = true)
    def end   = copy(isStart = false)
    def withTitle(t: String) = copy(whens = whens.withTitle(t))

    def stepsNumbers = whens.stepsNumbers :+ thenExtractors.toList.size

    /**
     * utility methods to build steps
     * extractors, mappers and verifications are sorted in the order of definition
     */
    private def givenExtractorsList = givens.givenExtractors.toList.reverse.map(_.asInstanceOf[StepParser[_]])
    private def whenExtractorsList  = whens.whenExtractors.toList.reverse.map(_.asInstanceOf[StepParser[_]])
    private def whenMappersList     = whens.mappers.toList.reverse
    private def thenExtractorsList  = thenExtractors.toList.reverse.map(_.asInstanceOf[StepParser[_]])
    private def verificationsList   = verifications.toList.reverse

    /** @return the values of all executed steps */
    private def stepsValues(steps: Seq[Fragment]) = steps.foldRight(HNil: HList) { (cur, res) => value(cur.executionResult) :: res }
    /** @return a -and- on all the execution results of steps */
    private def result(steps: Seq[Fragment]) = steps.foldRight(Success(): Result) { (cur, res) => cur.executionResult and res }
    /** execute a block of code depending on a previous result */
    private def executeIf(result: Result)(value: =>Any) = if (result.isSuccess) value else Skipped(" ")

    /** zip extractors, lines and steps to intercalate steps between texts fragments (and strip the lines of their delimiters if any */
    private def appendSteps(extractors: Seq[StepParser[_]], lines: Seq[String], steps: Seq[Fragment]): Seq[Fragment] =
      (extractors zip lines zip steps).map {
        case ((extractor: StepParser[_], l: String), s: Fragment) => Seq(fragmentFactory.Text(extractor.strip(l)+"\n"), s)
      }.flatten

    /** extract values from a line and execute a function */
    private def execute(previousResult: Result, extractor: StepParser[_], line: String)(f: Any => Any) =
      executeIf(previousResult) {
        extractLine(extractor, line) match {
          case DecoratedResult(t, _) => f(t)
          case other                 => other
        }
      }
    /** extract values from a line */
    private def extractLine(extractor: Any, line: String) =
      extractor.asInstanceOf[StepParser[Any]].parse(line).fold(e => Error(e), t => DecoratedResult(t, Success()))
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
 * The LastLines template takes the number of given / when / then steps of the scenario and associate the last (non-empty)
 * lines of the text with them
 */
case class LastLinesScriptTemplate() extends ScriptTemplate[Scenario, GivenWhenThenLines] {
  def lines(text: String, script: Scenario) = {
    // reverse the text and put the last empty lines aside
    val reversed = text.split("\n").reverse.toSeq
    val (emptyLines, ls) = reversed.span(_.trim.isEmpty)

    val linesBlocks = Seq(GivenLines(_), WhenLines(_), ThenLines(_))

    val grouped = (script.stepsNumbers zip linesBlocks).reverse.foldLeft((GivenWhenThenLines(), ls.reverse)) { (res, cur) =>
      val (gwtLines, remainingLines) = res
      val (stepsNumber, createBlock) = cur

      (gwtLines.prepend(createBlock(remainingLines.takeRight(stepsNumber))), remainingLines.dropRight(stepsNumber))
    }
    // add the remaining lines at the beginning and the empty lines at the end
    grouped._1.prepend(TextLines(grouped._2.mkString("\n"))).append(TextLines.create(emptyLines))
  }
}

/**
 * The bullet template associates lines starting with `*` and a keyword `given` / `when` ou `then`
 * to given / when / then steps
 */
case class BulletTemplate(bullet: String = "*") extends ScriptTemplate[Scenario, GivenWhenThenLines] {
  def lines(text: String, script: Scenario): GivenWhenThenLines = {
    text.split("\n").foldLeft(GivenWhenThenLines()) { (res, line) =>
      val firstBulletWord =
        (if (line.trim.startsWith(bullet)) line.trim.tail.trim.split(" ").headOption.getOrElse("") else "").toLowerCase

      val newLine = line.replace(bullet+" ", "")
      if      (firstBulletWord.startsWith("given")) res.append(GivenLines.create(newLine))
      else if (firstBulletWord.startsWith("when"))  res.append(WhenLines.create(newLine))
      else if (firstBulletWord.startsWith("then"))  res.append(ThenLines.create(newLine))
      else                                          res.append(TextLines(line))
    }
  }
}

/**
 * Set of extracted lines from some text which are either: simple text, given text, when text or then text
 */
case class GivenWhenThenLines(lines: Seq[GWTLines] = Seq()) extends ScriptLines {
  def prepend(ls: GWTLines) = (ls, lines.headOption) match {
    case (TextLines(l1), Some(TextLines(l2)))   => copy(lines = TextLines (l1 ++ l2) +: lines.tail)
    case (GivenLines(l1), Some(GivenLines(l2))) => copy(lines = GivenLines(l1 ++ l2) +: lines.tail)
    case (WhenLines(l1), Some(WhenLines(l2)))   => copy(lines = WhenLines (l1 ++ l2) +: lines.tail)
    case (ThenLines(l1), Some(ThenLines(l2)))   => copy(lines = ThenLines (l1 ++ l2) +: lines.tail)
    case (TextLines(l1), _)                     => if (l1.nonEmpty) copy(lines = ls +: lines) else this
    case _                                      => copy(lines = ls +: lines)
  }

  def append(ls: GWTLines) =
    (ls, lines.lastOption) match {
      case (TextLines(l1), Some(TextLines(l2)))   => copy(lines = lines.dropRight(1) :+ TextLines(l2 ++ l1))
      case (GivenLines(l1), Some(GivenLines(l2))) => copy(lines = lines.dropRight(1) :+ GivenLines(l2 ++ l1))
      case (WhenLines(l1), Some(WhenLines(l2)))   => copy(lines = lines.dropRight(1) :+ WhenLines(l2 ++ l1))
      case (ThenLines(l1), Some(ThenLines(l2)))   => copy(lines = lines.dropRight(1) :+ ThenLines(l2 ++ l1))
      case (TextLines(l1), _)                     => if (l1.nonEmpty) copy(lines = lines :+ ls) else this
      case _                                      => copy(lines = lines :+ ls)
    }
}

trait GWTLines
case class GivenLines(lines: Seq[String]) extends GWTLines
object GivenLines { def create(line: String): GivenLines = GivenLines(Seq(line)) }

case class WhenLines(lines: Seq[String]) extends GWTLines
object WhenLines { def create(line: String): WhenLines = WhenLines(Seq(line)) }

case class ThenLines(lines: Seq[String]) extends GWTLines
object ThenLines { def create(line: String): ThenLines = ThenLines(Seq(line)) }

case class TextLines(lines: String) extends GWTLines
object TextLines { def create(lines: Seq[String]): TextLines = TextLines(lines.mkString("\n")) }



