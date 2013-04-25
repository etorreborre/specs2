package examples

import org.specs2.Specification
import org.specs2.specification._
import org.specs2.execute._
import AsResult._
import shapeless._
import scalaz.std.list._
import scalaz.syntax.foldable._

class GivenWhenThenStepsSpec extends Specification with GivenWhenThenSteps { def is = s2"""

  A given-when-then example for a calculator
                                                  ${addition.start}
   Given the first number: {1}
   Given the second number: {2}
   With the addition operator {+}
   Then I should get: {3}
                                                  ${addition.end}

   TODO:

 * test all combinations
 * test with RegexParsers
 * strip the delimiters
 * change the extraction type to String => (T, String)
 * remove the FragmentsParsers with variable stuff
 * document
 * provide default DelimitedFragmentParsers anInt, twoInts, threeInts, aString, twoStrings,
   threeStrings, aDouble, twoDoubles, threeDoubles and combination thereof (+ dates, sequences, times?)
 * use only regexparsers with delimiters or not?

 """

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
    def given[T](f: String => T) = GWTGivens[T :: HNil, (String => T) :: HNil](title, f :: HNil)
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
    def given[T](f: String => T) = GWTGivens[T :: GT, (String => T) :: GTE](title, f :: givenExtractors)
    def when[T](f: String => T) = GWTWhensApply[T, GT, GTE, T :: HNil, HNil, (String => T) :: HNil, HNil](this, f :: HNil, HNil)

    def fragments(text: String): Fragments = Fragments.createList(Text(text))

    def start = copy(isStart = true)
    def end   = copy(isStart = false)
  }

  case class GWTWhens[GT <: HList, GTE <: HList, WT <: HList, WTR <: HList, WTE <: HList, WM <: HList](givens: GWTGivens[GT, GTE], whenExtractors: WTE, mappers: WM, isStart: Boolean = true) extends GWTSteps {
    def when[T](f: String => T)    = GWTWhensApply[T, GT, GTE, T :: WT, WTR, (String => T) :: WTE, WM](givens, f :: whenExtractors, mappers)
    def andThen[T](f: String => T) = GWTThensApply[T, GT, GTE, WT, WTR, WTE, WM, (String => T) :: HNil, HNil](GWTWhens(givens, whenExtractors, mappers), f :: HNil, HNil)

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

    def andThen[T](f: String => T) =
      GWTThensApply[T, GT, GTE, WT, WTR, WTE, WM, (String =>T) :: TTE, VE](GWTWhens(whens.givens, whens.whenExtractors, whens.mappers), f :: thenExtractors, verifications)

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

      val givenExtractorsList = givens.givenExtractors.toList(toListAny)
      val whenExtractorsList  = whens.whenExtractors.toList(toListAny)
      val thenExtractorsList  = thenExtractors.toList(toListAny)

      val (intro, givenLines, whenLines, thenLines) =
        (lines.dropRight(whenExtractorsList.size + thenExtractorsList.size + givenExtractorsList.size),
         lines.dropRight(whenExtractorsList.size + thenExtractorsList.size).takeRight(givenExtractorsList.size),
         lines.dropRight(thenExtractorsList.size).takeRight(whenExtractorsList.size),
         lines.takeRight(thenExtractorsList.size))

      val introFragments: Seq[Fragment] = intro.map(l => Text(l+"\n"))

      val givenSteps: Seq[Step] = (givenExtractorsList zip givenLines).view.map { case (extractor, line) => Step(extractor.asInstanceOf[String => Any](line)) }
      val givenFragments: Seq[Fragment] = (givenLines zip givenSteps).map { case (l: String, s: Step) => Text(l+"\n") ^ s }.flatMap(_.middle)

      lazy val givenStepsResults = givenSteps.foldRight(HNil: HList) { (cur, res) => value(cur.execute) :: res }
      val whenSteps =  (whenExtractorsList zip whenLines zip whens.mappers.toList(toListAny)).map { case ((extractor, line), mapper) =>
        Step {
          val extracted = extractor.asInstanceOf[String => Any](line)
          val values = extracted :: givenStepsResults
          mapper.asInstanceOf[Any => Any](values)
        }
      }

      val whenFragments: Seq[Fragment] = (whenLines zip whenSteps).map { case (l: String, s: Step) => Text(l+"\n") ^ s }.flatMap(_.middle)
      lazy val whenStepsResults = whenSteps.foldRight(HNil: HList) { (cur, res) => value(cur.execute) :: res }

      val thenExamples = (thenExtractorsList zip thenLines zip verifications.toList(toListAny)).map { case ((extractor, line), verify) =>
        Example(line,
        {
          val extracted = extractor.asInstanceOf[String => Any](line)
          val values = extracted :: whenStepsResults
          verify.asInstanceOf[Any => Any](values).asInstanceOf[Result]
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


}
