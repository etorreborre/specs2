package org.specs2
package text

class InterpolatedSpec extends Specification { def is = s2"""

 It is possible to retrieve, from an interpolated string, the expressions code, given the intermediate pieces of text

   when there is non-empty text surrounding all expressions       $e1
   when there is an starting empty text                           $e2
   when there is an ending empty text                             $e3
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

}
