package org.specs2
package specification

class SnippetsSpec extends Specification with Snippets { def is = s2"""

 These are examples on how to use the various snippet methods

   with the `snippet` method                                 $e1
   with the `snippet` method and cut comments                $e2
   with the `snippet` method and cut comments - 2 blocks     $e3

   with the `8<--` method - one block                        $e4
   with the `8<--` method - two blocks                       $e5

 It is possible to specify an offset to the snippet
   with the `snippet` method
     a positive offset                                       $e6
     a negative offset                                       $e7

   same thing with the `8<--` method
     a positive offset                                       $e8
     a negative offset                                       $e9

 Results can be displayed by using the `eval` method
   when using the `snippet` method                           $e10
   when using the `8<--` method                              $e11

                                                             """

  def e1 = s2""" code: ${ snippet {
var n = 0
n = 1
} } """.texts(1).t ===
  """|```
     |var n = 0
     |n = 1
     |```""".stripMargin


  def e2 = s2""" code: ${ snippet {
var n = 0
// 8<--
n = 1
// 8<--
n = 0
} } """.texts(1).t ===
    """|```
       |n = 1
       |```""".stripMargin

  def e3 = s2""" code: ${ snippet {
var n = 0
// 8<--
n = 1
// 8<--
n = 0
// 8<--
var i = 0
  } }
  """.texts(1).t ===
    """```
      |n = 1
      |var i = 0
      |```""".stripMargin

  def e4 = s2""" code: ${
var n = 0
`8<--`
n = 1
`8<--`
n = 0
`8<--`
} """.texts(1).t ===
    """|```
       |n = 1
       |```""".stripMargin

  def e5 = s2""" code: ${
var n = 0
`8<--`
n = 1
`8<--`
n = 0
`8<--`
var i = 0
`8<--`
} """.texts(1).t ===
    """|```
       |n = 1
       |var i = 0
       |```""".stripMargin

  def e6 = s2""" code: ${ snippet {
var n = 0
// 8<--
n = 1
// 8<--
n = 0
  }.offsetIs(2) } """.texts(1).t ===
    """|```
       |  n = 1
       |```""".stripMargin

  def e7 = s2""" code: ${ snippet {
  var n = 0
  // 8<--
  n = 1
  // 8<--
  n = 0
  }.offsetIs(-2) } """.texts(1).t ===
    """|```
       |n = 1
       |```""".stripMargin

  def e8 = s2""" code: ${
var n = 0
`8<--`
n = 1
`8<--`
n = 0
`8<--`(offset = 2)
  } """.texts(1).t ===
    """|```
       |  n = 1
       |```""".stripMargin

  def e9 = s2""" code: ${
  var n = 0
  `8<--`
  n = 1
  `8<--`
  n = 0
  `8<--`(offset = -2)
  } """.texts(1).t ===
    """|```
       |n = 1
       |```""".stripMargin

  def e10 = s2""" code: ${ snippet {
  var n = 1
  1 + n
  }.eval.offsetIs(-2) } """.texts.drop(1).take(2).map(_.t).mkString("\n") ===
    """|```
       |var n = 1
       |1 + n
       |```
       |```
       |> 2
       |```""".stripMargin

  def e11 = s2""" code: ${
`8<--`
var n = 1
(1 + n) eval
`8<--`
  } """.texts.drop(1).take(2).map(_.t).mkString("\n") ===
    """|```
       |var n = 1
       |(1 + n)
       |```
       |```
       |> 2
       |```""".stripMargin
}
