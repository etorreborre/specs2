package org.specs2.matcher

import util.parsing.combinator.Parsers
import util.parsing.input.{CharSequenceReader, Reader}

/**
 * Matchers for parser combinators
 */
trait ParserMatchers {
  import Matchers._
  // can't extend Parsers because Parser/Elem types would be different from ones we are testing
  val parsers: Parsers
  import parsers._

  // can't nest them in a ParseResultMatchers trait, because of an ambiguity error
  case class SuccessMatcher[T]() extends Matcher[ParseResult[T]] {
    def apply[S <: ParseResult[T]](x: Expectable[S]) = result(
      x.value.isInstanceOf[Success[T]],
      x.description,
      x.description+" isn't a Success",
      x)

    // e.g. andResultMust beEqualTo(3)
    def andResultMust(resultMatcher: => Matcher[T]): Matcher[ParseResult[T]] =
      this and (resultMatcher ^^ {
        _.asInstanceOf[Success[T]].result
      })

    // e.g. withResult equalTo(3)
    def withResult(resultMatcher: => Matcher[T]) = andResultMust(resultMatcher)

    // TODO: can't overload to withResult(expected: => T) Perhaps make one of overloads not call-by-name
  }

  case class NoSuccessMatcher[T, TNoSuccess <: NoSuccess : ClassManifest]() extends Matcher[ParseResult[T]] {
    def apply[S <: ParseResult[T]](x: Expectable[S]) = {
      val clazz = implicitly[ClassManifest[TNoSuccess]].erasure
      result(
        clazz.isInstance(x.value),
        x.description, // already includes "parse failed" or "parse error"
        x.description+" isn't a "+clazz.getSimpleName,
        x)
    }

    def andMsgMust(msgMatcher: => Matcher[String]): Matcher[ParseResult[T]] =
      this and (msgMatcher ^^ {
        _.asInstanceOf[TNoSuccess].msg
      })

    def withMsg(msgMatcher: => Matcher[String]) = andMsgMust(msgMatcher)
  }

  def beASuccess[T] = SuccessMatcher[T]

  def beAFailure[T] = NoSuccessMatcher[T, Failure]

  def beAnError[T] = NoSuccessMatcher[T, Error]

  // TODO: Doesn't have the correct type
  def succeedOn[T](input: Input) = beASuccess ^^ {(_: Parser[T])(input)}

  def failOn[T](input: Input) = beAFailure ^^ {(_: Parser[T])(input)}

  def errorOn[T](input: Input) = beAnError ^^ {(_: Parser[T])(input)}

  object ParserMatcherImplicits {
    implicit def useStringsAsInput(s: String): Reader[Char] = new CharSequenceReader(s)
  }
}
