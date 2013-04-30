package org.specs2
package specification

import execute._
import main.{ArgumentsArgs, Arguments}
import StandardFragments.{Backtab, Tab, Br, End}
import scalaz.Scalaz._
import control.{Functions, ImplicitParameters}
import control.Exceptions._
import data.TuplesToSeq
import matcher.MatchResult
import text.RegexExtractor

/**
 * This trait provides building blocks to create steps and examples from regular expression.
 *
 * It is used to implement a Given-When-Then way of describing systems.
 *
 * Fragments are created by adding a `Given` step to a `Text`:
 * {{{
 *  "name: ${user}" ^ givenName
 * }}}
 * This creates a PreStep object containing the current context (representing all the extracted values) and a list of
 * Fragments containing:
 *
 *  - the Text fragment: `Text("name: ${user}")`
 *  - a Step containing the extraction code to get the value delimited by `${}`
 *
 * Then, this PreStep object can be followed by another piece of Text to create a PreStepText object. This object merely
 * stores the additional Text fragment so that values can be extracted from it when a `When` step is added:
 * {{{
 *  // this creates a PreStepText object
 *  "name: ${user}" ^ givenName ^
 *  "age: ${38}"
 *
 *  // this creates a PreStep object
 *  "name: ${user}" ^ givenName ^
 *  "age: ${38}"    ^ thenAge ^
 * }}}
 *
 * Eventually, when a `Then` step is added, a sequence of PostStep/PostStepText objects is created. Those objects use
 * the current context and the results returned by the `Then` objects to create Examples.
 *
 * The last PostStep object contains the list of all fragments created by the Given/When/Then sequence:
 *
 *  - Text fragments
 *  - Steps
 *  - Examples
 *
 */
trait GivenWhenThen extends RegexStepsFactory with TuplesToSeq with FragmentsBuilder with SpecificationStringContext with ArgumentsArgs {
  /** at any point in time a regex sequence can be transformed as a sequence of Fragments */
  implicit def RegexFragmentToFragments(r: RegexFragment): Fragments = r.fs


  /**
   * implicit conversion to transform a Given[Y] to Given[X] when Y <: X
   */
  implicit def downcastGiven[X, Y <: X](gv: Given[Y]) = new Given[X](gv.regex) { def extract(s: String) = gv.extract(s) }

  /**
   * implicit conversion to transform a When[P, Q] to When[R, S] when R <: P and S >: Q
   */
  implicit def updowncastWhen[P, Q, R <: P, S >: Q](wh: When[P, Q]) =
    new When[R, S](wh.regex) { def extract(t: R, s: String): S = wh.extract(t, s) }

  /**
   * implicit conversion to transform a Then[Y] to Then[X] when Y <: X
   */
  implicit def upcastThen[X, Y <: X](th: Then[X]) = new Then[Y] { def extract(t: Y, s: String) = th.extract(t, s) }

  /**
   * Given / When / Then steps can be used in interpolated specifications but:
   *
   *  - the type safety between steps is lost
   *  - the restrictions on the order of given/when/then is lost
   */
  private var gwt: Seq[Either[Result, Any]] = Seq()
  private var thenSequence = false
  private def makeError: PartialFunction[Throwable, Result] = { case e: Throwable => Error(e) }
  private def executionIsOk = gwt.forall(_.isRight)

  implicit def givenIsSpecPart[T](g: Given[T]): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = {
      fs append {
        g.strip(text) ^
          Step {
            gwt = (if (thenSequence) Seq() else gwt) :+ trye(g.extract(text))(makeError)
            if (thenSequence) thenSequence = false
          }
      }
    }
  }
  implicit def whenIsSpecPart[T, U](w: When[T, U]): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") = {
      fs append {
        w.strip(text) ^
        Step {
          gwt = if (executionIsOk) {
            if (gwt.size > 1) Seq(trye(w.extract(gwt.map(_.right.get).asInstanceOf[T], text))(makeError))
            else              gwt.headOption.flatMap(_.right.toOption.map(v => trye(w.extract(v.asInstanceOf[T], text))(makeError))).toSeq
          } else gwt
        }
      }
    }
  }
  implicit def thenIsSpecPart[T](t: Then[T]): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String = "") =  {
      lazy val result = {
        thenSequence = true
        if (executionIsOk) gwt.lastOption.map(w => trye(t.extract(w.right.get.asInstanceOf[T], text))(makeError))
        else               gwt.lastOption
      }
      val texts = text.split("\n")
      val first = texts.dropRight(1).mkString("", "\n", "\n")
      val spaces = texts.last.takeWhile(Seq(' ', '\n').contains)
      val indent = spaces.dropRight(2).mkString
      val before = first + indent

      fs append {
        before ^ t.strip(text.trim) ! {
          result.getOrElse(Left(Failure("Can not call a Then step if there is no preceding Given step"))) match {
            case Right(r) => r.asInstanceOf[Result]
            case Left(e)  => e
          }
        }
      }
    }
  }

}

trait RegexStepsFactory extends ImplicitParameters {
  /** factory method to create a Given or a Then element from a regex */
  def readAs(regex: String) = new ReadAs(regex)
  /** factory method to create a Given or a Then element from a regex, using a regex denoting groups to extract */
  def groupAs(groupRegex: String) = new ReadAs(groups = "("+groupRegex+")")

  /** This class creates Given or Then extractors from a regular expression and a function */
  class ReadAs(regex: String = "", groups: String = "") {
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

    def and[T](f: String => T) = new Given[T](regex, groups) { def extract(text: String) = { f(extract1(text)) } }
    def and[T](f: (String, String) => T) = new Given[T](regex, groups) { def extract(text: String) = { f.tupled(extract2(text)) } }
    def and[T](f: (String, String, String) => T) = new Given[T](regex, groups) { def extract(text: String) = { f.tupled(extract3(text)) } }
    def and[T](f: (String, String, String, String) => T) = new Given[T](regex, groups) { def extract(text: String) = { f.tupled(extract4(text)) } }
    def and[T](f: (String, String, String, String, String) => T) = new Given[T](regex, groups) { def extract(text: String) = { f.tupled(extract5(text)) } }
    def and[T](f: (String, String, String, String, String, String) => T) = new Given[T](regex, groups) { def extract(text: String) = { f.tupled(extract6(text)) } }
    def and[T](f: (String, String, String, String, String, String, String) => T) = new Given[T](regex, groups) { def extract(text: String) = { f.tupled(extract7(text)) } }
    def and[T](f: (String, String, String, String, String, String, String, String) => T) = new Given[T](regex, groups) { def extract(text: String) = { f.tupled(extract8(text)) } }
    def and[T](f: (String, String, String, String, String, String, String, String, String) => T) = new Given[T](regex, groups) { def extract(text: String) = { f.tupled(extract9(text)) } }
    def and[T](f: (String, String, String, String, String, String, String, String, String, String) => T) = new Given[T](regex, groups) { def extract(text: String) = { f.tupled(extract10(text)) } }
    def and[T](f: Seq[String] => T)(implicit p1: ImplicitParam, p2: ImplicitParam) = new Given[T](regex, groups) { def extract(text: String)  = f(extractAll(text)) }

    private def extractValue[T] = (_:MatchResult[T]).expectable.value
    def and[T](f: String => MatchResult[T])(implicit p: ImplicitParam, p1: ImplicitParam1)                                                                            : Given[T] = and(f andThen extractValue)
    def and[T](f: (String, String) => MatchResult[T])(implicit p: ImplicitParam, p2: ImplicitParam2)                                                                  : Given[T] = and(Function.untupled(f.tupled andThen extractValue))
    def and[T](f: (String, String, String) => MatchResult[T])(implicit p: ImplicitParam, p3: ImplicitParam3)                                                          : Given[T] = and(Function.untupled(f.tupled andThen extractValue))
    def and[T](f: (String, String, String, String) => MatchResult[T])(implicit p: ImplicitParam, p4: ImplicitParam4)                                                  : Given[T] = and(Function.untupled(f.tupled andThen extractValue))
    def and[T](f: (String, String, String, String, String) => MatchResult[T])(implicit p: ImplicitParam, p5: ImplicitParam5)                                          : Given[T] = and(Function.untupled(f.tupled andThen extractValue))
    def and[T](f: (String, String, String, String, String, String) => MatchResult[T])(implicit p: ImplicitParam, p6: ImplicitParam6)                                  : Given[T] = and(Functions.untupled(f.tupled andThen extractValue))
    def and[T](f: (String, String, String, String, String, String, String) => MatchResult[T])(implicit p: ImplicitParam, p7: ImplicitParam7)                          : Given[T] = and(Functions.untupled(f.tupled andThen extractValue))
    def and[T](f: (String, String, String, String, String, String, String, String) => MatchResult[T])(implicit p: ImplicitParam, p8: ImplicitParam8)                  : Given[T] = and(Functions.untupled(f.tupled andThen extractValue))
    def and[T](f: (String, String, String, String, String, String, String, String, String) => MatchResult[T])(implicit p: ImplicitParam, p9: ImplicitParam9)          : Given[T] = and(Functions.untupled(f.tupled andThen extractValue))
    def and[T](f: (String, String, String, String, String, String, String, String, String, String) => MatchResult[T])(implicit p: ImplicitParam, p10: ImplicitParam10): Given[T] = and(Functions.untupled(f.tupled andThen extractValue))
    def and[T](f: Seq[String] => MatchResult[T])(implicit p: ImplicitParam, p2: ImplicitParam2)                                                                       : Given[T] = and(f andThen extractValue)(p, p)


    def and[T, S](f: T => String => S) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t)(extract1(text)) } }
    def and[T, S](f: T => (String, String) => S)(implicit p: ImplicitParam2) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).tupled(extract2(text)) } }
    def and[T, S](f: T => (String, String, String) => S)(implicit p: ImplicitParam3) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).tupled(extract3(text)) } }
    def and[T, S](f: T => (String, String, String, String) => S)(implicit p: ImplicitParam4) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).tupled(extract4(text)) } }
    def and[T, S](f: T => (String, String, String, String, String) => S)(implicit p: ImplicitParam5) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).tupled(extract5(text)) } }
    def and[T, S](f: T => (String, String, String, String, String, String) => S)(implicit p: ImplicitParam6) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).tupled(extract6(text)) } }
    def and[T, S](f: T => (String, String, String, String, String, String, String) => S)(implicit p: ImplicitParam7) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).tupled(extract7(text)) } }
    def and[T, S](f: T => (String, String, String, String, String, String, String, String) => S)(implicit p: ImplicitParam8) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).tupled(extract8(text)) } }
    def and[T, S](f: T => (String, String, String, String, String, String, String, String, String) => S)(implicit p: ImplicitParam9) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).tupled(extract9(text)) } }
    def and[T, S](f: T => (String, String, String, String, String, String, String, String, String, String) => S)(implicit p: ImplicitParam10) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).tupled(extract10(text)) } }
    def and[T, S](f: T => Seq[String] => S)(implicit p: ImplicitParam) = new When[T, S](regex, groups) { def extract(t: T, text: String) = { f(t).apply(extractAll(text)) } }

    def and[T,S](f: T => String => MatchResult[S])(implicit p: ImplicitParam, p1: ImplicitParam1)                                                                            : When[T,S] = and((t:T) => f(t) andThen extractValue)
    def and[T,S](f: T => (String, String) => MatchResult[S])(implicit p: ImplicitParam, p2: ImplicitParam2)                                                                  : When[T,S] = and((t:T) => Function.untupled (f(t).tupled andThen extractValue))(p2)
    def and[T,S](f: T => (String, String, String) => MatchResult[S])(implicit p: ImplicitParam, p3: ImplicitParam3)                                                          : When[T,S] = and((t:T) => Function.untupled (f(t).tupled andThen extractValue))(p3)
    def and[T,S](f: T => (String, String, String, String) => MatchResult[S])(implicit p: ImplicitParam, p4: ImplicitParam4)                                                  : When[T,S] = and((t:T) => Function.untupled (f(t).tupled andThen extractValue))(p4)
    def and[T,S](f: T => (String, String, String, String, String) => MatchResult[S])(implicit p: ImplicitParam, p5: ImplicitParam5)                                          : When[T,S] = and((t:T) => Function.untupled (f(t).tupled andThen extractValue))(p5)
    def and[T,S](f: T => (String, String, String, String, String, String) => MatchResult[S])(implicit p: ImplicitParam, p6: ImplicitParam6)                                  : When[T,S] = and((t:T) => Functions.untupled(f(t).tupled andThen extractValue))(p6)
    def and[T,S](f: T => (String, String, String, String, String, String, String) => MatchResult[S])(implicit p: ImplicitParam, p7: ImplicitParam7)                          : When[T,S] = and((t:T) => Functions.untupled(f(t).tupled andThen extractValue))(p7)
    def and[T,S](f: T => (String, String, String, String, String, String, String, String) => MatchResult[S])(implicit p: ImplicitParam, p8: ImplicitParam8)                  : When[T,S] = and((t:T) => Functions.untupled(f(t).tupled andThen extractValue))(p8)
    def and[T,S](f: T => (String, String, String, String, String, String, String, String, String) => MatchResult[S])(implicit p: ImplicitParam, p9: ImplicitParam9)          : When[T,S] = and((t:T) => Functions.untupled(f(t).tupled andThen extractValue))(p9)
    def and[T,S](f: T => (String, String, String, String, String, String, String, String, String, String) => MatchResult[S])(implicit p: ImplicitParam, p10: ImplicitParam10): When[T,S] = and((t:T) => Functions.untupled(f(t).tupled andThen extractValue))(p10)
    def and[T,S](f: T => Seq[String] => MatchResult[S])(implicit p1: ImplicitParam1, p: ImplicitParam)                                                                   : When[T,S] = and((t:T) => f(t) andThen extractValue)(p)

    def apply[R : AsResult](f: String => R) = andThen[R, Unit]((u: Unit) => f)
    def apply[R : AsResult](f: (String, String) => R) = andThen[R, Unit]((u: Unit) => f)
    def apply[R : AsResult](f: (String, String, String) => R) = andThen[R, Unit]((u: Unit) => f)
    def apply[R : AsResult](f: (String, String, String, String) => R) = andThen[R, Unit]((u: Unit) => f)
    def apply[R : AsResult](f: (String, String, String, String, String) => R) = andThen[R, Unit]((u: Unit) => f)
    def apply[R : AsResult](f: (String, String, String, String, String, String) => R) = andThen[R, Unit]((u: Unit) => f)
    def apply[R : AsResult](f: (String, String, String, String, String, String, String) => R) = andThen[R, Unit]((u: Unit) => f)
    def apply[R : AsResult](f: (String, String, String, String, String, String, String, String) => R) = andThen[R, Unit]((u: Unit) => f)
    def apply[R : AsResult](f: (String, String, String, String, String, String, String, String, String) => R) = andThen[R, Unit]((u: Unit) => f)
    def apply[R : AsResult](f: (String, String, String, String, String, String, String, String, String, String) => R) = andThen((u: Unit) => f)
    def apply[R](f: Seq[String] => R)(implicit r: AsResult[R], p: ImplicitParam) = andThen[R, Unit]((u: Unit) => f)(r, p)

    def andThen[R, T](f: T => String => R)(implicit r: AsResult[R]) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t)(extract1(text))) }
    def andThen[R, T](f: T => (String, String) => R)(implicit r: AsResult[R], p: ImplicitParam2) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t).tupled(extract2(text))) }
    def andThen[R, T](f: T => (String, String, String) => R)(implicit r: AsResult[R], p: ImplicitParam3) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t).tupled(extract3(text))) }
    def andThen[R, T](f: T => (String, String, String, String) => R)(implicit r: AsResult[R], p: ImplicitParam4) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t).tupled(extract4(text))) }
    def andThen[R, T](f: T => (String, String, String, String, String) => R)(implicit r: AsResult[R], p: ImplicitParam5) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t).tupled(extract5(text))) }
    def andThen[R, T](f: T => (String, String, String, String, String, String) => R)(implicit r: AsResult[R], p: ImplicitParam6) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t).tupled(extract6(text))) }
    def andThen[R, T](f: T => (String, String, String, String, String, String, String) => R)(implicit r: AsResult[R], p: ImplicitParam7) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t).tupled(extract7(text))) }
    def andThen[R, T](f: T => (String, String, String, String, String, String, String, String) => R)(implicit r: AsResult[R], p: ImplicitParam8) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t).tupled(extract8(text))) }
    def andThen[R, T](f: T => (String, String, String, String, String, String, String, String, String) => R)(implicit r: AsResult[R], p: ImplicitParam9) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t).tupled(extract9(text))) }
    def andThen[R, T](f: T => (String, String, String, String, String, String, String, String, String, String) => R)(implicit r: AsResult[R], p: ImplicitParam10) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t).tupled(extract10(text))) }
    def andThen[R, T](f: T => Seq[String] => R)(implicit r: AsResult[R], p: ImplicitParam) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = AsResult(f(t)(extractAll(text))) }
  }

}

private[specs2]
object GivenWhenThen extends GivenWhenThen {
  def toResult[T](context: =>Either[Result, (T, Result)]) = {
    context match {
      case Left(l)  => l
      case Right((t, r)) => r
    }
  }
  def toContext[T](context: =>Either[Result, (T, Result)]): Either[Result, T] = {
    context match {
      case Left(l)  => Left(l)
      case Right((t, r)) => Right(t)
    }
  }
}
import GivenWhenThen._

trait RegexFragment {
  type RegexType <: RegexFragment
  def fs: Fragments
  def add(f: Fragment): RegexType
  def ^(f: Text)        = add(f)
  def ^(f: Br)          = add(f)
  def ^(f: Tab)         = add(f)
  def ^(f: Backtab)     = add(f)
  def ^(f: End)         = fs.add(f)
  def ^(a: Arguments)   = fs.add(a)
  def ^(fs2: Fragments) = fs.add(fs2.middle)
}

private[specs2] case class PreStep[T](context: () => Either[Result, T], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStep[T]
  def ^(toExtract: String) = new PreStepText(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep(context, fs.add(f))
}

private[specs2] case class PreStep2[T1, T2](context: () => Either[Result, (T1, T2)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStep2[T1, T2]
  def ^(toExtract: String) = new PreStepText2(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep2(context, fs.add(f))
}

private[specs2] case class PreStep3[T1, T2, T3](context: () => Either[Result, (T1, T2, T3)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStep3[T1, T2, T3]
  def ^(toExtract: String) = new PreStepText3(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep3(context, fs.add(f))
}

private[specs2] case class PreStep4[T1, T2, T3, T4](context: () => Either[Result, (T1, T2, T3, T4)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStep4[T1, T2, T3, T4]
  def ^(toExtract: String) = new PreStepText4(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep4(context, fs.add(f))
}

private[specs2] case class PreStep5[T1, T2, T3, T4, T5](context: () => Either[Result, (T1, T2, T3, T4, T5)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStep5[T1, T2, T3, T4, T5]
  def ^(toExtract: String) = new PreStepText5(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep5(context, fs.add(f))
}

private[specs2] case class PreStep6[T1, T2, T3, T4, T5, T6](context: () => Either[Result, (T1, T2, T3, T4, T5, T6)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStep6[T1, T2, T3, T4, T5, T6]
  def ^(toExtract: String) = new PreStepText6(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep6(context, fs.add(f))
}

private[specs2] case class PreStep7[T1, T2, T3, T4, T5, T6, T7](context: () => Either[Result, (T1, T2, T3, T4, T5, T6, T7)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStep7[T1, T2, T3, T4, T5, T6, T7]
  def ^(toExtract: String) = new PreStepText7(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep7(context, fs.add(f))
}

private[specs2] case class PreStep8[T1, T2, T3, T4, T5, T6, T7, T8](context: () => Either[Result, (T1, T2, T3, T4, T5, T6, T7, T8)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStep8[T1, T2, T3, T4, T5, T6, T7, T8]
  def ^(toExtract: String) = new PreStepText8(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep8(context, fs.add(f))
}

private[specs2] case class PreStepText[T](text: String, context: () => Either[Result, T], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStepText[T]
  def ^[R](step: Given[R]): PreStep2[T, R] = {
    lazy val pair = (context() |@| step.extractContext(text))((_,_))
    new PreStep2(() => pair, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(pair)))
  }
  def ^[R](step: When[T, R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^[R, S](step: When[Seq[S], R])(implicit ev: T => Seq[S]) = {
    lazy val extracted = step.extractContext(context().right.map(ev), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^[R, S](step: Then[Seq[S]])(implicit ev: T => Seq[S]) = {
    lazy val extracted = step.extractContext(context().right.map(ev), text)
    new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def ^(step: Then[T]) = {
   lazy val extracted = step.extractContext(context(), text)
   new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText(text, context, fs.add(f))
}

private[specs2] case class PreStepText2[T1, T2](text: String, context: () => Either[Result, (T1, T2)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStepText2[T1, T2]
  def ^[R](step: Given[R]): PreStep3[T1, T2, R] = {
    lazy val tuple = (context() |@| step.extractContext(text))((t, r) => (t._1,t._2,r))
    new PreStep3(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^[R, T](step: When[Seq[T], R])(implicit ev1: T1 <:< T, ev2: T2 <:< T) = {
    lazy val extracted = step.extractContext(context().right.map(t => tupleToSeq2(t)), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText2(text, context, fs.add(f))
}

private[specs2] case class PreStepText3[T1, T2, T3](text: String, context: () => Either[Result, (T1, T2, T3)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment with ImplicitParameters {
  type RegexType = PreStepText3[T1, T2, T3]
  def ^[R](step: Given[R]): PreStep4[T1, T2, T3, R] = {
    lazy val tuple = (context() |@| step.extractContext(text))((t, r) => (t._1,t._2,t._3,r))
    new PreStep4(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^[R, T](step: When[Seq[T], R])(implicit ev1: T1 <:< T, ev2: T2 <:< T, ev3: T3 <:< T) = {
    lazy val extracted = step.extractContext(context().right.map(t => tupleToSeq3(t)), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText3(text, context, fs.add(f))
}

private[specs2] case class PreStepText4[T1, T2, T3, T4](text: String, context: () => Either[Result, (T1, T2, T3, T4)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStepText4[T1, T2, T3, T4]
  def ^[R](step: Given[R]): PreStep5[T1, T2, T3, T4, R] = {
    lazy val tuple = (context() |@| step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,r))
    new PreStep5(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^[R, T](step: When[Seq[T], R])(implicit ev1: T1 <:< T, ev2: T2 <:< T, ev3: T3 <:< T, ev4: T4 <:< T) = {
    lazy val extracted = step.extractContext(context().right.map(t => tupleToSeq4(t)), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText4(text, context, fs.add(f))
}

private[specs2] case class PreStepText5[T1, T2, T3, T4, T5](text: String, context: () => Either[Result, (T1, T2, T3, T4, T5)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStepText5[T1, T2, T3, T4, T5]
  def ^[R](step: Given[R]): PreStep6[T1, T2, T3, T4, T5, R] = {
    lazy val tuple = (context() |@| step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,t._5,r))
    new PreStep6(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4, T5), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^[R, T](step: When[Seq[T], R])(implicit ev1: T1 <:< T, ev2: T2 <:< T, ev3: T3 <:< T, ev4: T4 <:< T, ev5: T5 <:< T) = {
    lazy val extracted = step.extractContext(context().right.map(t => tupleToSeq5(t)), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4, T5)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText5(text, context, fs.add(f))
}

private[specs2] case class PreStepText6[T1, T2, T3, T4, T5, T6](text: String, context: () => Either[Result, (T1, T2, T3, T4, T5, T6)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStepText6[T1, T2, T3, T4, T5, T6]
  def ^[R](step: Given[R]): PreStep7[T1, T2, T3, T4, T5, T6, R] = {
    lazy val tuple = (context() |@| step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,t._5,t._6,r))
    new PreStep7(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4, T5, T6), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^[R, T](step: When[Seq[T], R])(implicit ev1: T1 <:< T, ev2: T2 <:< T, ev3: T3 <:< T, ev4: T4 <:< T, ev5: T5 <:< T, ev6: T6 <:< T) = {
    lazy val extracted = step.extractContext(context().right.map(t => tupleToSeq6(t)), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4, T5, T6)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText6(text, context, fs.add(f))
}

private[specs2] case class PreStepText7[T1, T2, T3, T4, T5, T6, T7](text: String, context: () => Either[Result, (T1, T2, T3, T4, T5, T6, T7)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStepText7[T1, T2, T3, T4, T5, T6, T7]
  def ^[R](step: Given[R]): PreStep8[T1, T2, T3, T4, T5, T6, T7, R] = {
    lazy val tuple = (context() |@| step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,t._5,t._6,t._7,r))
    new PreStep8(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4, T5, T6, T7), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^[R, T](step: When[Seq[T], R])(implicit ev1: T1 <:< T, ev2: T2 <:< T, ev3: T3 <:< T, ev4: T4 <:< T, ev5: T5 <:< T, ev6: T6 <:< T, ev7: T7 <:< T) = {
    lazy val extracted = step.extractContext(context().right.map(t => tupleToSeq7(t)), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4, T5, T6, T7)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText7(text, context, fs.add(f))
}

private[specs2] case class PreStepText8[T1, T2, T3, T4, T5, T6, T7, T8](text: String, context: () => Either[Result, (T1, T2, T3, T4, T5, T6, T7, T8)], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PreStepText8[T1, T2, T3, T4, T5, T6, T7, T8]
  def ^[R](step: Given[R]): PreStep8[T1, T2, T3, T4, T5, T6, T7, (T8, R)] = {
    lazy val tuple = (context() |@| step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,t._5,t._6,t._7,(t._8,r)))
    new PreStep8(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4, T5, T6, T7, T8), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^[R, T](step: When[Seq[T], R])(implicit ev1: T1 <:< T, ev2: T2 <:< T, ev3: T3 <:< T, ev4: T4 <:< T, ev5: T5 <:< T, ev6: T6 <:< T, ev7: T7 <:< T, ev8: T8 <:< T) = {
    lazy val extracted = step.extractContext(context().right.map(t => tupleToSeq8(t)), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4, T5, T6, T7, T8)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText8(text, context, fs.add(f))
}



private[specs2] case class PostStep[T](context: () => Either[Result, T], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PostStep[T]
  def ^(toExtract: String) = new PostStepText(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PostStep(context, fs.add(f))
}

private[specs2] case class PostStepText[T](text: String, context: () => Either[Result, T], fs: Fragments)(implicit exampleFactory: ExampleFactory) extends RegexFragment {
  type RegexType = PostStepText[T]
  def ^(step: Then[T]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(exampleFactory.newExample(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PostStepText(text, context, fs.add(f))
}


/**
 * This step can start a sequence of Given / When / Then.
 *
 * It must define the extract function creating a value of type T from the extracted values
 */
abstract class Given[T](var regex: String = "", var groupRegex: String = RegexExtractor.DEFAULT_REGEX) extends RegexExtractor[Unit, T](regex, groupRegex) {
  /** if the extraction goes wrong, then an Error is propagated */
  private[specs2] def extractContext(text: String): Either[Result, T] = ResultExecution.executeEither(extract(text))(identity)

  def extract(text: String): T

  def strip(f: Fragment) = f match {
    case Text(t) => Text(RegexExtractor.strip(t))
    case other   => other
  }
}

/** implicit conversions to create Given objects */
object Given extends ImplicitParameters {
  implicit def function1ToGiven[T](f: String => T): Given[T] = new Given[T] { def extract(text: String) = f(extract1(text))  }
  implicit def function2ToGiven[T](f: (String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract2(text))  }
  implicit def function3ToGiven[T](f: (String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract3(text))  }
  implicit def function4ToGiven[T](f: (String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract4(text))  }
  implicit def function5ToGiven[T](f: (String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract5(text))  }
  implicit def function6ToGiven[T](f: (String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract6(text))  }
  implicit def function7ToGiven[T](f: (String, String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract7(text))  }
  implicit def function8ToGiven[T](f: (String, String, String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract8(text))  }
  implicit def function9ToGiven[T](f: (String, String, String, String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract9(text))  }
  implicit def function10ToGiven[T](f:(String, String, String, String, String, String, String, String, String, String) => T): Given[T] = new Given[T] { def extract(text: String) = f.tupled(extract10(text))  }
  implicit def functionSeqToGiven[T](f: Seq[String] => T)(implicit p: ImplicitParam): Given[T] = new Given[T] { def extract(text: String) = f(extractAll(text))  }

  private def extractValue[T] = (_:MatchResult[T]).expectable.value
  implicit def function1ResultToGiven[T](f: String => MatchResult[T])                                                                          : Given[T] = function1ToGiven (f andThen extractValue)
  implicit def function2ResultToGiven[T](f: (String, String) => MatchResult[T])                                                                : Given[T] = function2ToGiven (Function.untupled(f.tupled andThen extractValue))
  implicit def function3ResultToGiven[T](f: (String, String, String) => MatchResult[T])                                                        : Given[T] = function3ToGiven (Function.untupled(f.tupled andThen extractValue))
  implicit def function4ResultToGiven[T](f: (String, String, String, String) => MatchResult[T])                                                : Given[T] = function4ToGiven (Function.untupled(f.tupled andThen extractValue))
  implicit def function5ResultToGiven[T](f: (String, String, String, String, String) => MatchResult[T])                                        : Given[T] = function5ToGiven (Function.untupled(f.tupled andThen extractValue))
  implicit def function6ResultToGiven[T](f: (String, String, String, String, String, String) => MatchResult[T])                                : Given[T] = function6ToGiven (Functions.untupled(f.tupled andThen extractValue))
  implicit def function7ResultToGiven[T](f: (String, String, String, String, String, String, String) => MatchResult[T])                        : Given[T] = function7ToGiven (Functions.untupled(f.tupled andThen extractValue))
  implicit def function8ResultToGiven[T](f: (String, String, String, String, String, String, String, String) => MatchResult[T])                : Given[T] = function8ToGiven (Functions.untupled(f.tupled andThen extractValue))
  implicit def function9ResultToGiven[T](f: (String, String, String, String, String, String, String, String, String) => MatchResult[T])        : Given[T] = function9ToGiven (Functions.untupled(f.tupled andThen extractValue))
  implicit def function10ResultToGiven[T](f:(String, String, String, String, String, String, String, String, String, String) => MatchResult[T]): Given[T] = function10ToGiven(Functions.untupled(f.tupled andThen extractValue))
  implicit def functionSeqResultToGiven[T](f: Seq[String] => MatchResult[T])(implicit p: ImplicitParam)                                        : Given[T] = functionSeqToGiven(f andThen extractValue)(p)
}
/**
 * This step define conditions in a sequence of Given / When / Then.
 *
 * It must define the extract function taking the previous state of extracted values, P, and creating a new state
 * of type T from the extracted values
 */
abstract class When[P, T](val regex: String = "", val groupRegex: String = RegexExtractor.DEFAULT_REGEX) extends RegexExtractor[P, T](regex, groupRegex) {
  /**
   * if the previous extraction went wrong, then a Skipped result is propagated.
   * Otherwise if the current extraction goes wrong, then an Error is propagated
   */
  private[specs2] def extractContext(p: Either[Result, P], text: String): Either[Result, T] = p match {
    case Left(l)  => Left(Skipped(l.message))
    case Right(r) => trye(extract(r, text))((e:Exception) => Error(e))
  }
  def extract(p: P, text: String): T
}
/** implicit conversions to create When objects */
object When extends ImplicitParameters {
  implicit def function1ToWhen[T, S](f: T => String => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t)(extract1(text))  }
  implicit def function2ToWhen[T, S](f: T => (String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract2(text))  }
  implicit def function3ToWhen[T, S](f: T => (String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract3(text))  }
  implicit def function4ToWhen[T, S](f: T => (String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract4(text))  }
  implicit def function5ToWhen[T, S](f: T => (String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract5(text)) }
  implicit def function6ToWhen[T, S](f: T => (String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract6(text)) }
  implicit def function7ToWhen[T, S](f: T => (String, String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract7(text)) }
  implicit def function8ToWhen[T, S](f: T => (String, String, String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract8(text)) }
  implicit def function9ToWhen[T, S](f: T => (String, String, String, String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract9(text)) }
  implicit def function10ToWhen[T, S](f: T => (String, String, String, String, String, String, String, String, String, String) => S): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t).tupled(extract10(text)) }
  implicit def functionSeqToWhen[T, S](f: T => Seq[String] => S)(implicit p: ImplicitParam): When[T, S] = new When[T, S] { def extract(t: T, text: String) = f(t)(extractAll(text))  }

  private def extractValue[T] = (_:MatchResult[T]).expectable.value
  implicit def function1ResultToWhen[T,S]  (f: T => String => MatchResult[S]): When[T,S]                                                                            = function1ToWhen  ((t:T)=> f(t) andThen extractValue)
  implicit def function2ResultToWhen[T, S] (f: T => (String, String) => MatchResult[S]): When[T, S]                                                                 = function2ToWhen  ((t:T)=>Function.untupled (f(t).tupled andThen extractValue))
  implicit def function3ResultToWhen[T, S] (f: T => (String, String, String) => MatchResult[S]): When[T, S]                                                         = function3ToWhen  ((t:T)=>Function.untupled (f(t).tupled andThen extractValue))
  implicit def function4ResultToWhen[T, S] (f: T => (String, String, String, String) => MatchResult[S]): When[T, S]                                                 = function4ToWhen  ((t:T)=>Function.untupled (f(t).tupled andThen extractValue))
  implicit def function5ResultToWhen[T, S] (f: T => (String, String, String, String, String) => MatchResult[S]): When[T, S]                                         = function5ToWhen  ((t:T)=>Function.untupled (f(t).tupled andThen extractValue))
  implicit def function6ResultToWhen[T, S] (f: T => (String, String, String, String, String, String) => MatchResult[S]): When[T, S]                                 = function6ToWhen  ((t:T)=>Functions.untupled(f(t).tupled andThen extractValue))
  implicit def function7ResultToWhen[T, S] (f: T => (String, String, String, String, String, String, String) => MatchResult[S]): When[T, S]                         = function7ToWhen  ((t:T)=>Functions.untupled(f(t).tupled andThen extractValue))
  implicit def function8ResultToWhen[T, S] (f: T => (String, String, String, String, String, String, String, String) => MatchResult[S]): When[T, S]                 = function8ToWhen  ((t:T)=>Functions.untupled(f(t).tupled andThen extractValue))
  implicit def function9ResultToWhen[T, S] (f: T => (String, String, String, String, String, String, String, String, String) => MatchResult[S]): When[T, S]         = function9ToWhen  ((t:T)=>Functions.untupled(f(t).tupled andThen extractValue))
  implicit def function10ResultToWhen[T, S](f: T => (String, String, String, String, String, String, String, String, String, String) => MatchResult[S]): When[T, S] = function10ToWhen ((t:T)=>Functions.untupled(f(t).tupled andThen extractValue))
  implicit def functionSeqResultToWhen[T, S](f: T => Seq[String] => MatchResult[S])(implicit p: ImplicitParam): When[T, S]                                          = functionSeqToWhen  ((t:T)=> f(t) andThen extractValue)(p)

}

/**
 * This step define checks in a sequence of Given / When / Then.
 *
 * It must define the extract function taking the state of extracted values, T, and return a `Result`
 */
abstract class Then[T](regex: String = "", val groupRegex: String = RegexExtractor.DEFAULT_REGEX) extends RegexExtractor[Either[Result, T], (T, Result)](regex, groupRegex) {
  /**
   * if the previous extraction went wrong, then a Skipped result is propagated.
   * Otherwise if the current extraction goes wrong, then an Error is propagated
   */
  private[specs2] def extractContext(t: Either[Result, T], text: String): Either[Result, (T, Result)] = t match {
    case Left(l)  => Left(Skipped(l.message))
    case Right(r) => trye((r, extract(r, text)))((e:Exception) => Error(e))
  }
  def extract(t: T, text: String): Result
}
/** implicit conversions to create Then objects */
object Then extends ImplicitParameters {
  implicit def function1ToThen[T, R : AsResult](f: T => String => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t)(extract1(text)))  }
  implicit def function2ToThen[T, R : AsResult](f: T => (String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract2(text)))  }
  implicit def function3ToThen[T, R : AsResult](f: T => (String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract3(text)))  }
  implicit def function4ToThen[T, R : AsResult](f: T => (String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract4(text)))  }
  implicit def function5ToThen[T, R : AsResult](f: T => (String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract5(text)))  }
  implicit def function6ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract6(text)))  }
  implicit def function7ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract7(text)))  }
  implicit def function8ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract8(text)))  }
  implicit def function9ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract9(text)))  }

  implicit def function10ToThen[T, R : AsResult](f: T => (String, String, String, String, String, String, String, String, String, String) => R): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t).tupled(extract10(text)))  }
  implicit def functionSeqToThen[T, R](f: T => Seq[String] => R)(implicit r: AsResult[R], p: ImplicitParam): Then[T] = new Then[T] { def extract(t: T, text: String) = AsResult(f(t)(extractAll(text)))  }
}

abstract class GivenThen(regex: String= "") extends RegexExtractor[String, Result](regex) {
  def extract(text: String): Result
}

class GivenThenFunction[R : AsResult](f: PartialFunction[Any, R], regex: String= "") extends GivenThen(regex) {
  def extract(text: String): Result = RegexExtractor.extract(text, { case a => AsResult(f(a)) }, regexToUse)
}
object so {
  def apply[R : AsResult](f: PartialFunction[Any, R]): GivenThen = new GivenThenFunction(f)
}
