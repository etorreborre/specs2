package org.specs2
package guide
package matchers

import form.Card

import scala.util.parsing.combinator._

object ParserMatchers extends UserGuideCard with UserGuideVariables {
  def title = "Parser"
  def text =  s2"""
Scala provides a parsing library using [parser combinators](http://www.scala-lang.org/api/current/scala/util/parsing/combinator/Parsers.html).

To specify your own parsers you need to:

 * extend the `org.specs2.matcher.ParserMatchers` trait
 * associate the `val parsers` variable with your parsers definition
 * use the `beASuccess`, `beAFailure`, `succeedOn`, `failOn`, `errorOn` matchers to specify the results of parsing input
 strings. `beAPartialSuccess`, `be aPartialSuccess`, `succeedOn.partially` will allow a successful match only on part of the input
 * use `haveSuccessResult` and `haveFailureMsg` to specify what happens *only* on success or failure. Those matchers accept
 a String or a matcher so that
 . `haveSuccessResult("r") <==> haveSuccessResult(beMatching(".*r.*") ^^ ((_:Any).toString)`
 . `haveFailingMsg("m") <==> haveFailingMsg(beMatching(".*r.*"))`

For example, specifying a Parser for numbers could look like this: ${snippet{

import NumberParsers.{error, number}

import scala.util.parsing.combinator.RegexParsers

class ParserSpec extends Specification with matcher.ParserMatchers {  def is = s2"""
  Parsers for numbers

    beASuccess and succeedOn check if the parse succeeds
    ${ number("1") must beASuccess }
    ${ number("1i") must beAPartialSuccess }
    ${ number must succeedOn("12") }
    ${ number must succeedOn("12ab").partially }
    ${ number must succeedOn("12").withResult(12) }
    ${ number must succeedOn("12").withResult(equalTo(12)) }
    ${ number("1") must haveSuccessResult("1") }

    beAFailure and failOn check if the parse fails
    ${ number must failOn("abc") }
    ${ number must failOn("abc").withMsg("string matching regex.*expected") }
    ${ number must failOn("abc").withMsg(matching(".*string matching regex.*expected.*")) }
    ${ number("i") must beAFailure }
    ${ number("i") must haveFailureMsg("i' found") }

    beAnError and errorOn check if the parser errors out completely
    ${ error must errorOn("") }
    ${ error("") must beAnError }
                                                                                """

  val parsers = NumberParsers
}
}}
"""
}

object NumberParsers extends RegexParsers {
  /** parse a number with any number of digits */
  val number: Parser[Int] = "\\d+".r ^^ {_.toInt}
  /** this parser returns an error */
  val error: Parser[String] = err("Error")
}
