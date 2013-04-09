package org.specs2
package text

import matcher.ParserMatchers
import specification.Snippets

class InterpolatedSpec extends Specification with ParserMatchers with Snippets { def is = s2"""

 It is possible to retrieve, from an interpolated string, the expressions code, given the intermediate pieces of text

  when there is non-empty text surrounding all expressions       $e1
    when there is an starting empty text                         $e2
    when there is an ending empty text                           $e3
    when there are several variables separated by spaces         $e4

  Interpolated expressions can be parsed
    when just using an identifier                                $e5
      when using accolades                                       $e6
      when using nested accolades                                $e7
    when using a quoted identifier                               $e8
                                                                 """

  def e1 = {
    val string =
      """|This is an interpolated string with one variable $e1
         |and another variable ${new BigTrait{}}
         |Then some other text""".stripMargin

    new Interpolated(string,
      Seq("This is an interpolated string with one variable ",
          "\nand another variable ",
          "\nThen some other text")).expressions === Seq("e1", "new BigTrait{}")
  }

  def e2 = {
    val string =
      """|$e1
         |middle $e2
         |end""".stripMargin

    new Interpolated(string,
      Seq("",
          "\nmiddle ",
          "\nend")).expressions === Seq("e1", "e2")
  }

  def e3 = {
    val string =
      """|start $e1
         |middle $e2""".stripMargin

    new Interpolated(string,
      Seq("start ",
          "\nmiddle ",
          "")).expressions === Seq("e1", "e2")
  }

  def e4 = {
    val string =
      """|start $e1 $e2 """.stripMargin

    new Interpolated(string,
      Seq("start ", " ", " ")).expressions === Seq("e1", "e2")
  }

  lazy val parsers = InterpolatedParsers

  def e5 = parsers.interpolatedVariable("$hello") must beASuccess
  def e6 = parsers.interpolatedVariable("${hello}") must beASuccess
  def e7 = parsers.interpolatedVariable("${ exp { other } }") must beASuccess.withResult("\\Q exp { other } \\E")
  def e8 = parsers.interpolatedVariable("${`hello world`}") must beASuccess.withResult("^hello world$")

}
