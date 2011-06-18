package org.specs2
package specification

import execute._
import specification.StandardFragments.{Br, End}
import main.Arguments
import matcher.MatchResult

/**
 * This trait provides building blocks to create steps and examples from regular expression.
 *
 * It is used to implement a Given-When-Then way of describing systems.
 *
 * Fragments are created by adding a `Given` step to a `Text`:
 *
 *  `"name: ${Eric}" ^ givenName`
 *
 * This creates a PreStep object containing the current context (representing all the extracted values) and a list of
 * Fragments containing:
 *
 *  * the Text fragment: `Text("name: ${Eric}")`
 *  * a Step containing the extraction code to get the value delimited by `${}`
 *
 * Then, this PreStep object can be followed by another piece of Text to create a PreStepText object. This object merely
 * stores the additional Text fragment so that values can be extracted from it when a `When` step is added:
 *
 *  // this creates a PreStepText object
 *  `"name: ${Eric}" ^ givenName ^`
 *  `"age: ${38}"`
 *
 *  // this creates a PreStep object
 *  `"name: ${Eric}" ^ givenName ^`
 *  `"age: ${38}"    ^ thenAge ^`
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

  trait RegexFragment {
    type RegexType <: RegexFragment
    val fs: Fragments
    def add(f: Fragment): RegexType
    def ^(f: Text): RegexType = add(f)
    def ^(f: Br): RegexType = add(f)
    def ^(f: End) = fs.add(f)
    def ^(a: Arguments) = fs.add(a)
    def ^(fs2: Fragments) = fs.add(fs2)
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
      new PreStep(() => extracted, fs.add(Text(step.strip(text))).add(Step.fromEither(extracted)))
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

  private def toResult[T](context: =>Either[Result, (T, Result)]) = {
    context match {
      case Left(l)  => l
      case Right((t, r)) => r
    }
  }
  private def toContext[T](context: =>Either[Result, (T, Result)]): Either[Result, T] = {
    context match {
      case Left(l)  => Left(l)
      case Right((t, r)) => Right(t)
    }
  }
}