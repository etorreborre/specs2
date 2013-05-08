package org.specs2
package matcher

import util.parsing.combinator.Parsers
import util.parsing.input.{CharSequenceReader, Reader}
import scalaz.Scalaz._
import text.Plural._
import text.Quote._
import scala.reflect.ClassTag
import MatchResultLogicalCombinators._
import scala.collection.GenTraversableOnce

/**
* Matchers for parser combinators
*
* When this trait is inherited the parsers variable needs to be defined.
*
* by @alexey_r
*/
trait ParserMatchers extends ParserBaseMatchers with ParserBeHaveMatchers

private[specs2]
trait ParserBaseMatchers extends TraversableMatchers {
  // can't extend Parsers because Parser/Elem types would be different from ones we are testing
  val parsers: Parsers
  import parsers.{Success => PSuccess, Failure => PFailure, Error => PError, _}

  /** match if the input is successfully parsed */
  def beASuccess[T] = ParseSuccessMatcher[T, ParseResult[T]](identity _)
  /** match if the input is successfully, but partially, parsed*/
  def beAPartialSuccess[T] = ParseSuccessMatcher[T, ParseResult[T]](identity _).partially
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
  /** match if the parse successful => matcher ok */
  def haveSuccessResult[T](m: Matcher[T]): Matcher[ParseResult[T]] =
    ParseSuccessMatcher[T, ParseResult[T]](identity _).andThenWithResult(m)
  /** match if the parse successful => string match ok */
  def haveSuccessResult[T](result: String): Matcher[ParseResult[T]] =
    haveSuccessResult(new BeMatching(".*"+result+".*") ^^ ((_:Any).toString))
  /** match if the parse fails => matcher ok */
  def haveFailureMsg[T](m: Matcher[String]): Matcher[ParseResult[T]] =
    ParseNoSuccessMatcher[T, ParseResult[T], PFailure](identity _).andThenWithMsg(m)
  /** match if the parse fails => string match ok */
  def haveFailureMsg[T](result: String): Matcher[ParseResult[T]] =
    haveFailureMsg(new BeMatching(".*"+result+".*"))


  private[specs2] trait ParseResultMatcher[T, TMatchee] extends Matcher[TMatchee] {
    val parseResult: TMatchee => ParseResult[T]

    def apply0(s: Expectable[ParseResult[T]]): MatchResult[ParseResult[T]]

    def apply[S <: TMatchee](s: Expectable[S]) = apply0(s.map(parseResult)).map(_ => s.value)

    protected def remaining(next: Input) = {
      val size = next.source.length - next.offset
      size.qty("character") + " remaining: " + q(next.source.subSequence(next.offset, next.source.length()))
    }
  }

  case class ParseSuccessMatcher[T, TMatchee](parseResult: TMatchee => ParseResult[T], isPartial: Boolean = false) extends ParseResultMatcher[T, TMatchee] {
    def partially = copy(isPartial = true)

    def apply0(s: Expectable[ParseResult[T]]) = {
      s.value match {
        case PSuccess(_, next) if next.atEnd || isPartial   =>
          Matcher.result(true, s.description, s.description+" isn't a Success", s)
        case PSuccess(_, next) if !next.atEnd && !isPartial =>
          Matcher.result(false, s.description,
                         s.description+" is a Success but the input was not completely parsed. "+ remaining(next), s)
        case _                                                =>
          Matcher.result(false, s.description, s.description+" isn't a Success", s)
      }
    }

    /** check if the parsed value is as expected as a regexp*/
    def withResult(result: String): Matcher[TMatchee] = withResult(new BeMatching(".*"+result+".*") ^^ ((_:Any).toString))
    /** check if the parsed value is as expected */
    def withResult(result: ExpectedParsedResult[T]): Matcher[TMatchee] = withResult(new BeEqualTo(result.t))

    /** check if the parsed value is as expected, using a matcher */
    def withResult(check: T => MatchResult[_]): Matcher[TMatchee] = new Matcher[TMatchee] {
      def apply[S <: TMatchee](s: Expectable[S]) = {
        val pResult = parseResult(s.value)
        lazy val resultMatcherResult: MatchResult[ParseResult[T]] = pResult match {
          case PSuccess(result, _) => check(result).map(_ => pResult)
          case _                   => MatchFailure("Parse succeeded", "Parse didn't succeed", s.map(pResult))
        }

        (apply0(Expectable(pResult)) and resultMatcherResult).map(_ => s.value)
      }
    }

    /** check if the parsed value is as expected, using a matcher */
    def withResult[U >: T](resultMatcher: Matcher[U]): Matcher[TMatchee] = new Matcher[TMatchee] {
      def apply[S <: TMatchee](s: Expectable[S]) = {
        val pResult = parseResult(s.value)
        lazy val resultMatcherResult: MatchResult[ParseResult[T]] = pResult match {
          case PSuccess(result, _) => resultMatcher(Expectable(result)).map(_ => pResult)
          case _                   => MatchFailure("Parse succeeded", "Parse didn't succeed", s.map(pResult))
        }

        (apply0(Expectable(pResult)) and resultMatcherResult).map(_ => s.value)
      }
    }

    protected[specs2]
    def andThenWithResult(resultMatcher: Matcher[T]): Matcher[TMatchee] = new Matcher[TMatchee] {
      def apply[S <: TMatchee](s: Expectable[S]) = {
        val pResult = parseResult(s.value)
        lazy val resultMatcherResult: MatchResult[ParseResult[T]] = pResult match {
          case PSuccess(result, _) => resultMatcher(Expectable(result)).map(_ => pResult)
          case _ => MatchSuccess("Parse succeeded", "Parse didn't succeed", s.map(pResult))
        }
        resultMatcherResult.map(_ => s.value)
      }
    }

  }

  case class ParseNoSuccessMatcher[T, TMatchee, TNoSuccess <: NoSuccess : ClassTag]
      (parseResult: TMatchee => ParseResult[T]) extends ParseResultMatcher[T, TMatchee] {
    val clazz = implicitly[ClassTag[TNoSuccess]].runtimeClass

    def apply0(s: Expectable[ParseResult[T]]) = {
      s.value match {
        case PSuccess(_, next) if !next.atEnd =>
          Matcher.result(true,
                         s.description+" is a Success and the input was not completely parsed. "+
                         remaining(next), s.description, s)
        case _                                                =>
          Matcher.result(clazz.isInstance(s.value), s.description, s.description+" isn't a "+clazz.getSimpleName, s)
      }
    }

    /** check if the failure message is as expected */
    def withMsg(msg: ExpectedParsedResult[String]): Matcher[TMatchee] = withMsg(new BeMatching(".*"+msg.t+".*"))

    /** check if the failure message is as expected, using a matcher */
    def withMsg(msgMatcher: Matcher[String]): Matcher[TMatchee] = new Matcher[TMatchee] {
      def apply[S <: TMatchee](s: Expectable[S]) = {
        val pResult = parseResult(s.value)
        lazy val msgMatcherResult = pResult match {
          case pNoSuccess: NoSuccess => msgMatcher(Expectable(pNoSuccess.msg))
          case _                     => MatchFailure("Parse failed", "Parse succeeded", s.map(pResult))
        }

        (apply0(Expectable(pResult)) and msgMatcherResult).map(_ => s.value)
      }
    }
    /** check if the failure message is as expected, only when the parse is failing */
    private[specs2]
    def andThenWithMsg(msgMatcher: Matcher[String]): Matcher[TMatchee] = new Matcher[TMatchee] {
      def apply[S <: TMatchee](s: Expectable[S]) = {
        val pResult = parseResult(s.value)
        lazy val msgMatcherResult = pResult match {
          case pNoSuccess: NoSuccess => msgMatcher(Expectable(pNoSuccess.msg))
          case _                     => MatchSuccess("Parse failed", "Parse succeeded", s.map(pResult))
        }
        msgMatcherResult.map(_ => s.value)
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
    def aSuccess                           = result(beASuccess)
    def aPartialSuccess                    = result(beAPartialSuccess)
    def aFailure                           = result(beAFailure)
    def aParseError                        = result(beAnError)
    def haveSuccessResult(s: String)       = result(outer.haveSuccessResult(s))
    def haveSuccessResult(m: Matcher[T])   = result(outer.haveSuccessResult(m))
    def haveFailureMsg(s: String)          = result(outer.haveFailureMsg(s))
    def haveFailureMsg(m: Matcher[String]) = result(outer.haveFailureMsg(m))
  }

  def aSuccess        = beASuccess
  def aPartialSuccess = beAPartialSuccess
  def aFailure        = beAFailure
  def aParseError     = beAnError

  implicit def toParserResultMatcherResult[T](result: MatchResult[Parser[T]]) = new ParserResultMatcherResult(result)
  class ParserResultMatcherResult[T](result: MatchResult[Parser[T]]) {
    def succeedOn(input: Input) = result(outer.succeedOn(input))
    def failOn(input: Input) = result(outer.failOn(input))
    def errorOn(input: Input) = result(outer.errorOn(input))
  }

}
/** This class is only used as a transient holder for the expected parsed value, to overcome overloaded method issues */
case class ExpectedParsedResult[+T](t: T) {
  override def toString = t.toString
}
object ExpectedParsedResult {
  implicit def toExpectedParsedResult[T](t: T): ExpectedParsedResult[T] = new ExpectedParsedResult(t)
}
