package org.specs2
package matcher

import util.parsing.combinator.Parsers
import util.parsing.input.{CharSequenceReader, Reader}

/**
 * Matchers for parser combinators
 */
trait ParserMatchers {
  import Matchers._
  import scalaz.Scalaz._
  // can't extend Parsers because Parser/Elem types would be different from ones we are testing
  val parsers: Parsers
  import parsers.{Success => PSuccess, Failure => PFailure, Error => PError, _}

  private[specs2] trait ParseResultMatcher[T, TMatchee] extends Matcher[TMatchee] {
    val parseResult: TMatchee => ParseResult[T]

    def apply0(x: Expectable[ParseResult[T]]): MatchResult[ParseResult[T]]

    def apply[S <: TMatchee](x: Expectable[S]) = apply0(x.map(parseResult)).map(_ => x.value)
  }

  case class ParseSuccessMatcher[T, TMatchee](parseResult: TMatchee => ParseResult[T]) extends ParseResultMatcher[T, TMatchee] {
    def apply0(x: Expectable[ParseResult[T]]) = Matcher.result(
      x.value.isInstanceOf[PSuccess[_]],
      x.description,
      x.description+" isn't a Success",
      x)

    // e.g. andResultMust beEqualTo(3)
    def withResult(resultMatcher: => Matcher[T]) = new Matcher[TMatchee] {
      def apply[S <: TMatchee](x: Expectable[S]) = {
        val pResult = parseResult(x.value)
        lazy val resultMatcherResult: MatchResult[ParseResult[T]] = pResult match {
          case PSuccess(result, _) => resultMatcher(Expectable(result)).map(_ => pResult)
          case _ => MatchFailure("Parse succeeded", "Parse didn't succeed", Expectable(pResult))
        }

        (apply0(Expectable(pResult)) and resultMatcherResult).map(_ => x.value)
      }
    }

    // e.g. withResult equalTo(3)
    def andResultMust(resultMatcher: => Matcher[T]) = withResult(resultMatcher)

    // TODO: can't overload to withResult(expected: => T) Perhaps make one of overloads not call-by-name
  }

  case class ParseNoSuccessMatcher[T, TMatchee, TNoSuccess <: NoSuccess : ClassManifest]
      (parseResult: TMatchee => ParseResult[T]) extends ParseResultMatcher[T, TMatchee] {
    val clazz = implicitly[ClassManifest[TNoSuccess]].erasure

    def apply0(x: Expectable[ParseResult[T]]) = Matcher.result(
      clazz.isInstance(x.value),
      x.description,
      x.description+" isn't a "+clazz.getSimpleName,
      x)

    def withMsg(msgMatcher: => Matcher[String]) = new Matcher[TMatchee] {
      def apply[S <: TMatchee](x: Expectable[S]) = {
        val pResult = parseResult(x.value)
        lazy val msgMatcherResult = pResult match {
          case pNoSuccess: NoSuccess => msgMatcher(Expectable(pNoSuccess.msg))
          case _ => MatchFailure("Parse has not succeeded", "Parse didn't not succeed correctly", Expectable(pResult))
        }

        (apply0(Expectable(pResult)) and msgMatcherResult).map(_ => x.value)
      }
    }

    def andMsgMust(msgMatcher: => Matcher[String]) = withMsg(msgMatcher)
  }

  def beASuccess[T] = ParseSuccessMatcher[T, ParseResult[T]](identity _)

  def beAFailure[T] = ParseNoSuccessMatcher[T, ParseResult[T], PFailure](identity _)

  def beAnError[T] = ParseNoSuccessMatcher[T, ParseResult[T], PError](identity _)

  def succeedOn[T](input: Input) = ParseSuccessMatcher[T, Parser[T]](_(input))

  def failOn[T](input: Input) = ParseNoSuccessMatcher[T, Parser[T], PFailure](_(input))

  def errorOn[T](input: Input) = ParseNoSuccessMatcher[T, Parser[T], PError](_(input))

  object ParserMatcherImplicits {
    implicit def useStringsAsInput(s: String): Reader[Char] = new CharSequenceReader(s)
  }
}
