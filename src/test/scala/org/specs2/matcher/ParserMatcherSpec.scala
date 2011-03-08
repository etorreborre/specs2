package org.specs2
package matcher

import util.parsing.combinator.RegexParsers

class ParserMatcherSpec extends SpecificationWithJUnit with ParserMatchers {
  import ParserMatcherImplicits._
  import ParsersUnderTest.number

  def is =
  """
  The ParserMatchers trait provides matchers for Parser and ParseResult instances.
  """^
  "succeedOn tests if the parser succeeds on the given input"^
  { number must succeedOn("12").withResult(equalTo(12)) }

  val parsers = ParsersUnderTest
}

object ParsersUnderTest extends RegexParsers {
  val number: Parser[Int] = """\d+""".r ^^ {_.toInt}
}