package org.specs2
package specification

class SnippetsSpec extends Specification with Snippets { def is = s2"""

 These are examples on how to use the various snippet methods

   with the `snippet` method                                 $e1
   with the `snippet` method and cut comments                ${e2.pendingUntilFixed}
   with the `snippet` method and cut comments - 2 blocks     $e3

   with the `8<--` method - one block                        $e4
   with the `8<--` method - two blocks                       $e5
                                                             """

  def e1 = s2""" code: ${ snippet {
var n = 0
n = 1
} }
  other
  """.texts(1).t ===
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
} }""".texts(1).t ===
    """```
      |n = 1
      |```""".stripMargin

  def e3 = pending
  /*

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
    """
      |n = 1
      |var i = 0
    """.stripMargin
*/
  def e4 = pending
  def e5 = pending

}
