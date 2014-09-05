package org.specs2
package matcher

import util.parsing.combinator.RegexParsers
import ParsersUnderTest.{ numbers, number, anyNumber, error }

class ParserMatchersSpec extends Spec with ParserMatchers with StringMatchers { def is = s2"""

The ParserMatchers trait provides matchers for Parser and ParseResult instances.

  succeedOn tests if the parser succeeds on the given input
  ${ number("1") must beASuccess }
  ${ number("1") must be aSuccess }
  ${ number("i") must not be aSuccess }
  ${ number must succeedOn("12ab").partially }
  ${ number must succeedOn("12").withResult(12) }
  ${ anyNumber must succeedOn("12").withResult(12) }
  ${ number must succeedOn("12ab").partially.withResult(12) }
  ${ number must succeedOn("12").withResult(equalTo(12)) }
  ${ numbers must succeedOn("12,13").withResult((l: List[Int]) => l must contain(12)) }
  ${ number must not succeedOn("abc") }
  ${ number must not(succeedOn("abc").withResult(equalTo(13))) }

  haveSuccessResult tests if the parser returns a given result when succeeding
  ${ number("1") must haveSuccessResult("1") }
  ${ number("1") must haveSuccessResult(equalTo(1)) }
  ${ number("1") must not haveSuccessResult(equalTo(2)) }
  ${ number("x") must haveSuccessResult("1") }
  ${ number("x") must haveSuccessResult(equalTo(1)) }

  failOn tests if the parser fails on the given input
  ${ number must failOn("abc").withMsg(matching(".*")) }
  ${ number must failOn("abc").withMsg("expected") }
  ${ number must not failOn("12") }
  ${ number must failOn("12a") }
  ${ (number must failOn("12a ")) returns
    "'[1.3] parsed: 12' is a Success and the input was not completely parsed. 2 characters remaining: 'a '" }
  ${ number("i") must beAFailure }
  ${ number("1") must not be aFailure }
  ${ number("i") must be aFailure }
  ${ number("i2") must be aFailure }
  ${ number("1i") must beAPartialSuccess }
  ${ number("1i") must be aPartialSuccess }

  haveFailureMsg tests if the parser returns a given message when failing
  ${ number("abc") must haveFailureMsg("a' found") }
  ${ number("abc") must haveFailureMsg(matching(".*expected.*")) }
  ${ number("abc") must not haveFailureMsg("xxxxx") }
  ${ number("x") must haveFailureMsg("x' found") }

  errorOn tests if the parser errors out completely
  ${ error must errorOn("") }
  ${ error("") must beAnError }
  ${ error("") must be aParseError }
  ${ number must not errorOn("abc") }
                                                                                                                        """

  val parsers = ParsersUnderTest
}

object ParsersUnderTest extends RegexParsers {
  /** parse a list of numbers */
  lazy val numbers: Parser[List[Int]] = rep1sep(number, ",")

  /** parse a number with any number of digits */
  lazy val number: Parser[Int] = """\d+""".r ^^ {_.toInt}

  /** parse a number as Any - see issue 63 */
  lazy val anyNumber: Parser[Any] = """\d+""".r ^^ {_.toInt}

  /** this parser returns an error */
  lazy val error: Parser[String] = err("Error")
}
