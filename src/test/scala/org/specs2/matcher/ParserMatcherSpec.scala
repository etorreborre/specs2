package org.specs2
package matcher

import util.parsing.combinator.RegexParsers

class ParserMatcherSpec extends SpecificationWithJUnit with ParserMatchers {
  import ParserMatcherImplicits._
  import ParsersUnderTest.{number, error}
  val parsers = ParsersUnderTest

  def is =
  """
  The ParserMatchers trait provides matchers for Parser and ParseResult instances.
  """^
  "succeedOn tests if the parser succeeds on the given input"^
  { number must succeedOn("12").withResult(equalTo(12)) }^
  { number must not(succeedOn("abc")) }^
  { number must not(succeedOn("abc").withResult(equalTo(13))) }^
  "failOn tests if the parser fails on the given input"^
  { number must failOn("abc").withMsg(matching(".*")) }^
  { number must not(failOn("12")) }^
  "errorOn tests if the parser errors out completely"^
  { error must errorOn("") }^
  { number must not(errorOn("abc")) }

}

object ParsersUnderTest extends RegexParsers {
  val number: Parser[Int] = """\d+""".r ^^ {_.toInt}

  val error: Parser[String] = err("Error")
}