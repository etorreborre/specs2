package org.specs2
package specification

class SnippetsSpec extends Specification with Snippets { def is = s2"""

 These are examples on how to use the various snippet methods

   with the `snippet` method                                 $e1
   with the `snippet` method and cut comments                $e2
   with the `snippet` method and cut comments - 2 blocks     $e3

 It is possible to specify an offset to the snippet
   with the `snippet` method
     a positive offset                                       $e4
     a negative offset                                       $e5

 Results can be displayed by using the `eval` method
   when using the `snippet` method                           $e6
                                                             """

  def e1 = s2""" code: ${ snippet {
var n = 0
n = 1
} } """.texts(1).t.trim ===
  """|```
     |var n = 0
     |n = 1
     |```""".stripMargin


  def e2 = s2""" code: ${ snippet {
// 8<--
var n = 0
// 8<--
n = 1
// 8<--
n = 0
} }""".texts(1).t.trim ===
    """|```
       |n = 1
       |```""".stripMargin

  def e3 = s2""" code: ${ snippet {
// 8<--
var n = 0
// 8<--
n = 1
// 8<--
n = 0
// 8<--
var i = 0
  } }""".texts(1).t.trim ===
    """```
      |n = 1
      |var i = 0
      |```""".stripMargin

  def e4 = s2""" code: ${ snippet {
// 8<--
var n = 0
// 8<--
n = 1
// 8<--
n = 0
  }.offsetIs(2) }""".texts(1).t.trim ===
    """|```
       |  n = 1
       |```""".stripMargin

  def e5 = s2""" code: ${ snippet {
  // 8<--
  var n = 0
  // 8<--
  n = 1
  // 8<--
  n = 0
  }.offsetIs(-2) }""".texts(1).t.trim ===
    """|```
       |n = 1
       |```""".stripMargin


  def e6 = s2""" code: ${ snippet {
  var n = 1
  1 + n
  }.eval.offsetIs(-2) }""".texts.drop(1).take(2).map(_.t.trim).mkString("\n") ===
    """|```
       |var n = 1
       |1 + n
       |```
       |```
       |> 2
       |```""".stripMargin
}
