package org.specs2
package specification

import execute._
import main.Arguments
import matcher.MatchResult
import specification.StandardFragments.{Backtab, Tab, Br, End}

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
*  * the Text fragment: <code>Text("name: ${Eric}")</code>
*  * a Step containing the extraction code to get the value delimited by <code>${}</code>
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
*  * Text fragments
*  * Steps
*  * Examples
*
*/
trait RegexSteps {
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

private[specs2] case class PreStepText[T](text: String, context: () => Either[Result, T], fs: Fragments) extends RegexFragment {
  type RegexType = PreStepText[T]
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
