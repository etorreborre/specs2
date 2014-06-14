package org.specs2
package guide
package old

import matchers.NumberParsers
import NumberParsers._

class ParserSpec extends Specification with matcher.ParserMatchers {  def is =
  "Parsers for numbers"                                                                      ^
                                                                                             p^
  "beASuccess and succeedOn check if the parse succeeds"                                     ^
  eg { number("1") must beASuccess }                                                         ^
  eg { number must succeedOn("12") }                                                         ^
  eg { number must succeedOn("12").withResult(12) }                                          ^
  eg { number must succeedOn("12").withResult(equalTo(12)) }                                 ^
  eg { number("1") must haveSuccessResult("1") }                                             ^
                                                                                             p^
  "beAFailure and failOn check if the parse fails"                                           ^
  eg { number must failOn("abc") }                                                           ^
  eg { number must failOn("abc").withMsg("string matching regex.*expected") }                ^
  eg { number must failOn("abc").withMsg(matching(".*string matching regex.*expected.*")) }  ^
  eg { number("i") must beAFailure }                                                         ^
  eg { number("i") must haveFailureMsg("i' found") }                                         ^
                                                                                             p^
  "beAnError and errorOn check if the parser errors out completely"                          ^
  eg { error must errorOn("") }                                                              ^
  eg { error("") must beAnError }                                                            ^
                                                                                             end

  val parsers = NumberParsers
}
