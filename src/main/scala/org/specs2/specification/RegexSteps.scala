package org.specs2
package specification

import execute._
import main.Arguments
import specification.StandardFragments.{Backtab, Tab, Br, End}
import internal.scalaz.Scalaz._
import control.ImplicitParameters

/**
 * This trait provides building blocks to create steps and examples from regular expression.
 *
 * It is used to implement a Given-When-Then way of describing systems.
 *
 * Fragments are created by adding a `Given` step to a `Text`:
 *
 *  <code>"name: ${user}" ^ givenName</code>
 *
 * This creates a PreStep object containing the current context (representing all the extracted values) and a list of
 * Fragments containing:
 *
 *  - the Text fragment: <code>Text("name: ${user}")</code>
 *  - a Step containing the extraction code to get the value delimited by <code>${}</code>
 *
 * Then, this PreStep object can be followed by another piece of Text to create a PreStepText object. This object merely
 * stores the additional Text fragment so that values can be extracted from it when a `When` step is added:
 * <code>
 *  // this creates a PreStepText object
 *  "name: ${user}" ^ givenName ^
 *  "age: ${38}"
 *
 *  // this creates a PreStep object
 *  "name: ${user}" ^ givenName ^
 *  "age: ${38}"    ^ thenAge ^
 * </code>
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
trait RegexSteps extends RegexStepsFactory {
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
    def apply(f: Seq[String] => Unit)(implicit p: ImplicitParam) = and[Unit](f)

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
    def and[T](f: Seq[String] => T)(implicit p: ImplicitParam, p1: ImplicitParam1) = new Given[T](regex, groups) { def extract(text: String)  = f(extractAll(text)) }

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

    def apply[R <% Result](f: String => R) = then[R, Unit]((u: Unit) => f)
    def apply[R <% Result](f: (String, String) => R) = then[R, Unit]((u: Unit) => f)
    def apply[R <% Result](f: (String, String, String) => R) = then[R, Unit]((u: Unit) => f)
    def apply[R <% Result](f: (String, String, String, String) => R) = then[R, Unit]((u: Unit) => f)
    def apply[R <% Result](f: (String, String, String, String, String) => R) = then[R, Unit]((u: Unit) => f)
    def apply[R <% Result](f: (String, String, String, String, String, String) => R) = then[R, Unit]((u: Unit) => f)
    def apply[R <% Result](f: (String, String, String, String, String, String, String) => R) = then[R, Unit]((u: Unit) => f)
    def apply[R <% Result](f: (String, String, String, String, String, String, String, String) => R) = then[R, Unit]((u: Unit) => f)
    def apply[R <% Result](f: (String, String, String, String, String, String, String, String, String) => R) = then[R, Unit]((u: Unit) => f)
    def apply[R <% Result](f: (String, String, String, String, String, String, String, String, String, String) => R) = then((u: Unit) => f)
    def apply[R](f: Seq[String] => R)(implicit r: R => Result, p: ImplicitParam) = then[R, Unit]((u: Unit) => f)(r, p)

    def then[R, T](f: T => String => R)(implicit r: R => Result) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t)(extract1(text)) }
    def then[R, T](f: T => (String, String) => R)(implicit r: R => Result, p: ImplicitParam2) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t).tupled(extract2(text)) }
    def then[R, T](f: T => (String, String, String) => R)(implicit r: R => Result, p: ImplicitParam3) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t).tupled(extract3(text)) }
    def then[R, T](f: T => (String, String, String, String) => R)(implicit r: R => Result, p: ImplicitParam4) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t).tupled(extract4(text)) }
    def then[R, T](f: T => (String, String, String, String, String) => R)(implicit r: R => Result, p: ImplicitParam5) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t).tupled(extract5(text)) }
    def then[R, T](f: T => (String, String, String, String, String, String) => R)(implicit r: R => Result, p: ImplicitParam6) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t).tupled(extract6(text)) }
    def then[R, T](f: T => (String, String, String, String, String, String, String) => R)(implicit r: R => Result, p: ImplicitParam7) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t).tupled(extract7(text)) }
    def then[R, T](f: T => (String, String, String, String, String, String, String, String) => R)(implicit r: R => Result, p: ImplicitParam8) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t).tupled(extract8(text)) }
    def then[R, T](f: T => (String, String, String, String, String, String, String, String, String) => R)(implicit r: R => Result, p: ImplicitParam9) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t).tupled(extract9(text)) }
    def then[R, T](f: T => (String, String, String, String, String, String, String, String, String, String) => R)(implicit r: R => Result, p: ImplicitParam10) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t).tupled(extract10(text)) }
    def then[R, T](f: T => Seq[String] => R)(implicit r: R => Result, p: ImplicitParam) = new Then[T](regex, groups) { def extract(t: T, text: String): Result = f(t)(extractAll(text)) }
  }
}

private[specs2]
object RegexSteps extends RegexSteps {
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
import RegexSteps._

private[specs2]
trait RegexFragment {
  type RegexType <: RegexFragment
  val fs: Fragments
  def add(f: Fragment): RegexType
  def ^(f: Text)        = add(f)
  def ^(f: Br)          = add(f)
  def ^(f: Tab)         = add(f)
  def ^(f: Backtab)     = add(f)
  def ^(f: End)         = fs.add(f)
  def ^(a: Arguments)   = fs.add(a)
  def ^(fs2: Fragments) = fs.add(fs2.middle)
}

private[specs2] case class PreStep[T](context: () => Either[Result, T], fs: Fragments) extends RegexFragment {
  type RegexType = PreStep[T]
  def ^(toExtract: String) = new PreStepText(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep(context, fs.add(f))
}

private[specs2] case class PreStep2[T1, T2](context: () => Either[Result, (T1, T2)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStep2[T1, T2]
  def ^(toExtract: String) = new PreStepText2(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep2(context, fs.add(f))
}

private[specs2] case class PreStep3[T1, T2, T3](context: () => Either[Result, (T1, T2, T3)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStep3[T1, T2, T3]
  def ^(toExtract: String) = new PreStepText3(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep3(context, fs.add(f))
}

private[specs2] case class PreStep4[T1, T2, T3, T4](context: () => Either[Result, (T1, T2, T3, T4)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStep4[T1, T2, T3, T4]
  def ^(toExtract: String) = new PreStepText4(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep4(context, fs.add(f))
}

private[specs2] case class PreStep5[T1, T2, T3, T4, T5](context: () => Either[Result, (T1, T2, T3, T4, T5)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStep5[T1, T2, T3, T4, T5]
  def ^(toExtract: String) = new PreStepText5(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep5(context, fs.add(f))
}

private[specs2] case class PreStep6[T1, T2, T3, T4, T5, T6](context: () => Either[Result, (T1, T2, T3, T4, T5, T6)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStep6[T1, T2, T3, T4, T5, T6]
  def ^(toExtract: String) = new PreStepText6(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep6(context, fs.add(f))
}

private[specs2] case class PreStep7[T1, T2, T3, T4, T5, T6, T7](context: () => Either[Result, (T1, T2, T3, T4, T5, T6, T7)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStep7[T1, T2, T3, T4, T5, T6, T7]
  def ^(toExtract: String) = new PreStepText7(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep7(context, fs.add(f))
}

private[specs2] case class PreStep8[T1, T2, T3, T4, T5, T6, T7, T8](context: () => Either[Result, (T1, T2, T3, T4, T5, T6, T7, T8)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStep8[T1, T2, T3, T4, T5, T6, T7, T8]
  def ^(toExtract: String) = new PreStepText8(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PreStep8(context, fs.add(f))
}

private[specs2] case class PreStepText[T](text: String, context: () => Either[Result, T], fs: Fragments) extends RegexFragment {
  type RegexType = PreStepText[T]
  def ^[R](step: Given[R]): PreStep2[T, R] = {
    lazy val pair = (context() <**> step.extractContext(text))((_,_))
    new PreStep2(() => pair, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(pair)))
  }
  def ^[R](step: When[T, R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[T]) = {
   lazy val extracted = step.extractContext(context(), text)
   new PostStep(() => toContext(extracted), fs.add(Example(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText(text, context, fs.add(f))
}

private[specs2] case class PreStepText2[T1, T2](text: String, context: () => Either[Result, (T1, T2)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStepText2[T1, T2]
  def ^[R](step: Given[R]): PreStep3[T1, T2, R] = {
    lazy val tuple = (context() <**> step.extractContext(text))((t, r) => (t._1,t._2,r))
    new PreStep3(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(Example(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText2(text, context, fs.add(f))
}

private[specs2] case class PreStepText3[T1, T2, T3](text: String, context: () => Either[Result, (T1, T2, T3)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStepText3[T1, T2, T3]
  def ^[R](step: Given[R]): PreStep4[T1, T2, T3, R] = {
    lazy val tuple = (context() <**> step.extractContext(text))((t, r) => (t._1,t._2,t._3,r))
    new PreStep4(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(Example(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText3(text, context, fs.add(f))
}

private[specs2] case class PreStepText4[T1, T2, T3, T4](text: String, context: () => Either[Result, (T1, T2, T3, T4)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStepText4[T1, T2, T3, T4]
  def ^[R](step: Given[R]): PreStep5[T1, T2, T3, T4, R] = {
    lazy val tuple = (context() <**> step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,r))
    new PreStep5(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(Example(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText4(text, context, fs.add(f))
}

private[specs2] case class PreStepText5[T1, T2, T3, T4, T5](text: String, context: () => Either[Result, (T1, T2, T3, T4, T5)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStepText5[T1, T2, T3, T4, T5]
  def ^[R](step: Given[R]): PreStep6[T1, T2, T3, T4, T5, R] = {
    lazy val tuple = (context() <**> step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,t._5,r))
    new PreStep6(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4, T5), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4, T5)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(Example(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText5(text, context, fs.add(f))
}

private[specs2] case class PreStepText6[T1, T2, T3, T4, T5, T6](text: String, context: () => Either[Result, (T1, T2, T3, T4, T5, T6)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStepText6[T1, T2, T3, T4, T5, T6]
  def ^[R](step: Given[R]): PreStep7[T1, T2, T3, T4, T5, T6, R] = {
    lazy val tuple = (context() <**> step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,t._5,t._6,r))
    new PreStep7(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4, T5, T6), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4, T5, T6)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(Example(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText6(text, context, fs.add(f))
}

private[specs2] case class PreStepText7[T1, T2, T3, T4, T5, T6, T7](text: String, context: () => Either[Result, (T1, T2, T3, T4, T5, T6, T7)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStepText7[T1, T2, T3, T4, T5, T6, T7]
  def ^[R](step: Given[R]): PreStep8[T1, T2, T3, T4, T5, T6, T7, R] = {
    lazy val tuple = (context() <**> step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,t._5,t._6,t._7,r))
    new PreStep8(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4, T5, T6, T7), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4, T5, T6, T7)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(Example(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText7(text, context, fs.add(f))
}

private[specs2] case class PreStepText8[T1, T2, T3, T4, T5, T6, T7, T8](text: String, context: () => Either[Result, (T1, T2, T3, T4, T5, T6, T7, T8)], fs: Fragments) extends RegexFragment {
  type RegexType = PreStepText8[T1, T2, T3, T4, T5, T6, T7, T8]
  def ^[R](step: Given[R]): PreStep8[T1, T2, T3, T4, T5, T6, T7, (T8, R)] = {
    lazy val tuple = (context() <**> step.extractContext(text))((t, r) => (t._1,t._2,t._3,t._4,t._5,t._6,t._7,(t._8,r)))
    new PreStep8(() => tuple, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(tuple)))
  }
  def ^[R](step: When[(T1, T2, T3, T4, T5, T6, T7, T8), R]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PreStep(() => extracted, fs.add(Backtab()).add(Text(step.strip(text))).add(Step.fromEither(extracted)))
  }
  def ^(step: Then[(T1, T2, T3, T4, T5, T6, T7, T8)]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(Example(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PreStepText8(text, context, fs.add(f))
}



private[specs2] case class PostStep[T](context: () => Either[Result, T], fs: Fragments) extends RegexFragment {
  type RegexType = PostStep[T]
  def ^(toExtract: String) = new PostStepText(toExtract, context, fs)
  def add(f: Fragment): RegexType = new PostStep(context, fs.add(f))
}

private[specs2] case class PostStepText[T](text: String, context: () => Either[Result, T], fs: Fragments) extends RegexFragment {
  type RegexType = PostStepText[T]
  def ^(step: Then[T]) = {
    lazy val extracted = step.extractContext(context(), text)
    new PostStep(() => toContext(extracted), fs.add(Example(step.strip(text), toResult(extracted))))
  }
  def add(f: Fragment): RegexType = new PostStepText(text, context, fs.add(f))
}
