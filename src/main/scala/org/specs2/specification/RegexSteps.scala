package org.specs2
package specification

import execute._
import specification.StandardFragments.{Br, End}

/**
 * This trait provides building block to create steps and examples from regular expression.
 *
 * It is used to implement a Given-When-Then way of describing systems.
 */
trait RegexSteps {

  /** at any point in time a regex sequence can be transformed as a sequence of Fragments */
  implicit def RegexFragmentToFragments(r: RegexFragment): Fragments = r.fs

  /** Regulaat any point in time a regex sequence can be transformed as a sequence of Fragments */
  abstract class RegexStep[P, T](regex: String) {
    def extractAll(text: String) = regex.r.unapplySeq(text).get
    def extract1(t: String) = (extractAll(t): @unchecked) match { case s1::_ => s1 }
    def extract2(t: String) = (extractAll(t): @unchecked) match { case s1::s2::_ => (s1,s2) }
    def extract3(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::_ => (s1,s2,s3) }
    def extract4(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::_ => (s1,s2,s3,s4) }
    def extract5(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::s5::_ => (s1,s2,s3,s4,s5) }
    def extract6(t: String) = (extractAll(t): @unchecked) match { case s1::s2::s3::s4::s5::s6::_ => (s1,s2,s3,s4,s5,s6) }
  }

  private def tryOrError[T](t: =>T): Either[Result, T] = try(Right(t)) catch { case (e:Exception) => Left(Error(e)) }

  abstract class Given[T](regex: String) extends RegexStep[Unit, T](regex) {
    def extractContext(text: String): Either[Result, T] = tryOrError(extract(text))
    def extract(text: String): T
  }
  abstract class When[P, T](regex: String) extends RegexStep[P, T](regex) {
    def extractContext(p: Either[Result, P], text: String): Either[Result, T] = p match {
      case Left(l)  => Left(Skipped(l.message))
      case Right(r) => tryOrError(extract(r, text))
    }
    def extract(p: P, text: String): T
  }

  abstract class Then[T](regex: String) extends RegexStep[Either[Result, T], (T, Result)](regex) {
    def extractContext(t: Either[Result, T], text: String): Either[Result, (T, Result)] = t match {
      case Left(l)  => Left(Skipped(l.message))
      case Right(r) => tryOrError((r, extract(r, text)))
    }
    def extract(t: T, text: String): Result
  }

  trait RegexFragment {
    type RegexType <: RegexFragment
    val fs: Fragments
    def add(f: Fragment): RegexType
    def ^(f: Text): RegexType = add(f)
    def ^(f: Br): RegexType = add(f)
    def ^(f: End) = fs.add(f)
    def ^(fs2: Fragments) = fs.add(fs2)
  }

  case class PreStep[T](context: () => Either[Result, T], fs: Fragments) extends RegexFragment {
    type RegexType = PreStep[T]
    def ^(toExtract: String) = new PreStepText(toExtract, context, fs)
    def add(f: Fragment): RegexType = new PreStep(context, fs.add(f))
  }

  case class PreStepText[T](text: String, context: () => Either[Result, T], fs: Fragments) extends RegexFragment {
    type RegexType = PreStepText[T]
    def ^[R](step: When[T, R]) = {
      lazy val extracted = step.extractContext(context(), text)
      new PreStep(() => extracted, fs.add(Text(text)).add(Step.fromEither(extracted)))
    }
    def ^(step: Then[T]) = {
     lazy val extracted = step.extractContext(context(), text)
     new PostStep(() => toContext(extracted), fs.add(Example(text, toResult(extracted))))
    }
    def add(f: Fragment): RegexType = new PreStepText(text, context, fs.add(f))
  }

  case class PostStep[T](context: () => Either[Result, T], fs: Fragments) extends RegexFragment {
    type RegexType = PostStep[T]
    def ^(toExtract: String) = new PostStepText(toExtract, context, fs)
    def add(f: Fragment): RegexType = new PostStep(context, fs.add(f))
  }

  case class PostStepText[T](text: String, context: () => Either[Result, T], fs: Fragments) extends RegexFragment {
    type RegexType = PostStepText[T]
    def ^(step: Then[T]) = {
      lazy val extracted = step.extractContext(context(), text)
      new PostStep(() => extracted, fs.add(Example(text, toResult(extracted))))
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