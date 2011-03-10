package org.specs2
package matcher

import util.parsing.combinator.RegexParsers
import ParsersUnderTest.{ number, error }

class ParserMatcherSpec extends SpecificationWithJUnit with ParserMatchers { def is =
                                                                                                                        """
The ParserMatchers trait provides matchers for Parser and ParseResult instances.
                                                                                                                        """^p^
  "succeedOn tests if the parser succeeds on the given input"                                                           ^
  { number("1") must beASuccess }                                                                                       ^
  { number("1") must be aSuccess }                                                                                      ^
  { number("i") must not be aSuccess }                                                                                  ^
  { number must succeedOn("12").withResult(12) }                                                                        ^
  { number must succeedOn("12").withResult(equalTo(12)) }                                                               ^
  { number must not succeedOn("abc") }                                                                                  ^
  { number must not(succeedOn("abc").withResult(equalTo(13))) }                                                         ^
                                                                                                                        p^
  "failOn tests if the parser fails on the given input"                                                                 ^
  { number must failOn("abc").withMsg(matching(".*")) }                                                                 ^
  { number must failOn("abc").withMsg("expected") }                                                                     ^
  { number must not failOn("12") }                                                                                      ^
  { number("i") must beAFailure }                                                                                       ^
  { number("1") must not be aFailure }                                                                                  ^
  { number("i") must be aFailure }                                                                                      ^
                                                                                                                        p^
  "errorOn tests if the parser errors out completely"                                                                   ^
  { error must errorOn("") }                                                                                            ^
  { error("") must beAnError }                                                                                          ^
  { error("") must be aParseError }                                                                                     ^
  { number must not errorOn("abc") }                                                                                    ^
                                                                                                                        end

  val parsers = ParsersUnderTest
}

object ParsersUnderTest extends RegexParsers {
  /** parse a number with any number of digits */
  val number: Parser[Int] = """\d+""".r ^^ {_.toInt}

  /** this parser returns an error */
  val error: Parser[String] = err("Error")
}