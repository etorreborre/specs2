package org.specs2
package matcher

import util.parsing.combinator.Parsers
import util.parsing.input.{CharSequenceReader, Reader}
import scalaz.Scalaz._

/**
 * Matchers for parser combinators
 *
 * When this trait is inherited the parsers variable needs to be defined.
 *
 * by @alexey_r
 */
trait ParserMatchers extends ParserBaseMatchers with ParserBeHaveMatchers

private[specs2]
trait ParserBaseMatchers {
  // can't extend Parsers because Parser/Elem types would be different from ones we are testing
  val parsers: Parsers
  import parsers.{Success => PSuccess, Failure => PFailure, Error => PError, _}

  /** match if the input is successfully parsed */
  def beASuccess[T] = ParseSuccessMatcher[T, ParseResult[T]](identity _)
  /** match if the input is not successfully parsed */
  def beAFailure[T] = ParseNoSuccessMatcher[T, ParseResult[T], PFailure](identity _)
  /** match if parsing the input raises an error */
  def beAnError[T] = ParseNoSuccessMatcher[T, ParseResult[T], PError](identity _)
  /** match if the input is successfully parsed */
  def succeedOn[T](input: Input) = ParseSuccessMatcher[T, Parser[T]](_(input))
  /** match if the input is not successfully parsed */
  def failOn[T](input: Input) = ParseNoSuccessMatcher[T, Parser[T], PFailure](_(input))
  /** match if parsing the input raises an error */
  def errorOn[T](input: Input) = ParseNoSuccessMatcher[T, Parser[T], PError](_(input))


  private[specs2] trait ParseResultMatcher[T, TMatchee] extends Matcher[TMatchee] {
    val parseResult: TMatchee => ParseResult[T]

    def apply0(s: Expectable[ParseResult[T]]): MatchResult[ParseResult[T]]

    def apply[S <: TMatchee](s: Expectable[S]) = apply0(s.map(parseResult)).map(_ => s.value)
  }

  case class ParseSuccessMatcher[T, TMatchee](parseResult: TMatchee => ParseResult[T]) extends ParseResultMatcher[T, TMatchee] {
    def apply0(s: Expectable[ParseResult[T]]) = Matcher.result(
      s.value.isInstanceOf[PSuccess[_]],
      s.description,
      s.description+" isn't a Success",
      s)

    def withResult(resultMatcher: Matcher[T]) = new Matcher[TMatchee] {
      def apply[S <: TMatchee](s: Expectable[S]) = {
        val pResult = parseResult(s.value)
        lazy val resultMatcherResult: MatchResult[ParseResult[T]] = pResult match {
          case PSuccess(result, _) => resultMatcher(Expectable(result)).map(_ => pResult)
          case _ => MatchFailure("Parse succeeded", "Parse didn't succeed", s.map(pResult))
        }

        (apply0(Expectable(pResult)) and resultMatcherResult).map(_ => s.value)
      }
    }

  }

  case class ParseNoSuccessMatcher[T, TMatchee, TNoSuccess <: NoSuccess : ClassManifest]
      (parseResult: TMatchee => ParseResult[T]) extends ParseResultMatcher[T, TMatchee] {
    val clazz = implicitly[ClassManifest[TNoSuccess]].erasure

    def apply0(s: Expectable[ParseResult[T]]) = Matcher.result(
      clazz.isInstance(s.value),
      s.description,
      s.description+" isn't a "+clazz.getSimpleName,
      s)

    def withMsg(msgMatcher: Matcher[String]) = new Matcher[TMatchee] {
      def apply[S <: TMatchee](s: Expectable[S]) = {
        val pResult = parseResult(s.value)
        lazy val msgMatcherResult = pResult match {
          case pNoSuccess: NoSuccess => msgMatcher(Expectable(pNoSuccess.msg))
          case _ => MatchFailure("Parse has not succeeded", "Parse didn't not succeed correctly", s.map(pResult))
        }

        (apply0(Expectable(pResult)) and msgMatcherResult).map(_ => s.value)
      }
    }
  }
  implicit def useStringsAsInput(s: String): Reader[Char] = new CharSequenceReader(s)
}

private[specs2]
trait ParserBeHaveMatchers { outer: ParserBaseMatchers =>
  import parsers.{Success => PSuccess, Failure => PFailure, Error => PError, _}
  
  implicit def toParsedResultMatcher[T](result: MatchResult[ParseResult[T]]) = new ParsedResultMatcher(result)
  class ParsedResultMatcher[T](result: MatchResult[ParseResult[T]]) {
    def aSuccess                = result(beASuccess)
    def aFailure                = result(beAFailure)
    def aParseError             = result(beAnError)
  }

  def aSuccess    = beASuccess
  def aFailure    = beAFailure
  def aParseError = beAnError

  implicit def toParserResultMatcherResult[T](result: MatchResult[Parser[T]]) = new ParserResultMatcherResult(result)
  class ParserResultMatcherResult[T](result: MatchResult[Parser[T]]) {
    def succeedOn(input: Input) = result(outer.succeedOn(input))
    def failOn(input: Input) = result(outer.failOn(input))
    def errorOn(input: Input) = result(outer.errorOn(input))
  }

}
