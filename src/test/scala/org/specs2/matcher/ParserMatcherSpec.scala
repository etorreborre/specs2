package org.specs2
package matcher

import util.parsing.combinator.RegexParsers
import ParsersUnderTest.{ number, error }

class ParserMatcherSpec extends Specification with ParserMatchers { def is =
                                                                                                                               """
The ParserMatchers trait provides matchers for Parser and ParseResult instances.
                                                                                                                               """^p^
  "succeedOn tests if the parser succeeds on the given input"                                                                  ^
  { number("1") must beASuccess }                                                                                              ^
  { number("1") must be aSuccess }                                                                                             ^
  { number("i") must not be aSuccess }                                                                                         ^
  { number must succeedOn("12ab").partially }                                                                                  ^
  { number must succeedOn("12").withResult(12) }                                                                               ^
  { number must succeedOn("12ab").partially.withResult(12) }                                                                   ^
  { number must succeedOn("12").withResult(equalTo(12)) }                                                                      ^
  { number must not succeedOn("abc") }                                                                                         ^
  { number must not(succeedOn("abc").withResult(equalTo(13))) }                                                                ^
                                                                                                                               p^
  "haveSuccessResult tests if the parser returns a given result when succeeding"                                               ^
  { number("1") must haveSuccessResult("1") }                                                                                  ^
  { number("1") must haveSuccessResult(equalTo(1)) }                                                                           ^
  { number("1") must not haveSuccessResult(equalTo(2)) }                                                                       ^
  { number("x") must haveSuccessResult("1") }                                                                                  ^
  { number("x") must haveSuccessResult(equalTo(1)) }                                                                           ^
                                                                                                                               p^
  "failOn tests if the parser fails on the given input"                                                                        ^
  { number must failOn("abc").withMsg(matching(".*")) }                                                                        ^
  { number must failOn("abc").withMsg("expected") }                                                                            ^
  { number must not failOn("12") }                                                                                             ^
  { number must failOn("12a") }                                                                                                ^
  { (number must failOn("12a")) returns "'[1.3] parsed: 12' is a Success and the input was not completely parsed: 12a" }       ^
  { number("i") must beAFailure }                                                                                              ^
  { number("1") must not be aFailure }                                                                                         ^
  { number("i") must be aFailure }                                                                                             ^
  { number("i2") must be aFailure }                                                                                            ^
  { number("1i") must beAPartialSuccess }                                                                                      ^
  { number("1i") must be aPartialSuccess }                                                                                     ^
                                                                                                                               p^
  "haveFailureMsg tests if the parser returns a given message when failing"                                                    ^
  { number("abc") must haveFailureMsg("a' found") }                                                                            ^
  { number("abc") must haveFailureMsg(matching(".*expected.*")) }                                                              ^
  { number("abc") must not haveFailureMsg("xxxxx") }                                                                           ^
  { number("x") must haveFailureMsg("x' found") }                                                                              ^
                                                                                                                               p^
  "errorOn tests if the parser errors out completely"                                                                          ^
  { error must errorOn("") }                                                                                                   ^
  { error("") must beAnError }                                                                                                 ^
  { error("") must be aParseError }                                                                                            ^
  { number must not errorOn("abc") }                                                                                           ^
                                                                                                                               end

  val parsers = ParsersUnderTest
}

object ParsersUnderTest extends RegexParsers {
  /** parse a number with any number of digits */
  val number: Parser[Int] = """\d+""".r ^^ {_.toInt}

  /** this parser returns an error */
  val error: Parser[String] = err("Error")
}