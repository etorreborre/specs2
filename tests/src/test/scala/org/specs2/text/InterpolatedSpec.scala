package org.specs2
package text

import specification._

class InterpolatedSpec extends Specification with Snippets { def is = s2"""

 It is possible to retrieve, from an interpolated string, the expressions code,
 given the intermediate pieces of text

 Expressions can be collected
  normal case                                          $e1
  when there is an starting empty text                 $e2
  when there is an ending empty text                   $e3
  when there are several variables separated by spaces $e4
"""

  def e1 = {
    val string =
      s"""|This is an interpolated string with one variable $$e1
          |and another variable $${ new BigTrait {} }
          |Then some other text""".stripMargin

    new Interpolated(string,
      Seq("This is an interpolated string with one variable ",
          "\nand another variable ",
          "\nThen some other text")).expressions === Seq("e1", " new BigTrait {} ")
  }

  def e2 = {
    val string =
      s"""|$$e1
          |middle $$e2
          |end""".stripMargin

    new Interpolated(string,
      Seq("",
          "\nmiddle ",
          "\nend")).expressions === Seq("e1", "e2")
  }

  def e3 = {
    val string =
      s"""|start $$e1
          |middle $$e2""".stripMargin

    new Interpolated(string,
      Seq("start ",
          "\nmiddle ",
          "")).expressions === Seq("e1", "e2")
  }

  def e4 = {
    val string =
      s"""|start $$e1 $$e2 """.stripMargin

    new Interpolated(string,
      Seq("start ", " ", " ")).expressions === Seq("e1", "e2")
  }

}
