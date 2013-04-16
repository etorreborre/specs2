package org.specs2
package specification

import matcher.DataTables

class SnippetsSpec extends Specification with Snippets with DataTables { def is = s2"""

 These are examples on how to use the various snippet methods

   with the `snippet` method                                 $e1
   with the `snippet` method and 2 lines                     $e2
   with the `snippet` method and cut comments                $e3
   with the `snippet` method and cut comments - 2 blocks     $e4

 It is possible to specify an offset to the snippet
   with the `snippet` method
     a positive offset                                       $e5
     a negative offset                                       $e6

 Results can be displayed by using the `eval` method
   when using the `snippet` method                           $e7

 It is also possible to capture code names
   for types (trait, classes,...)                            $e8
     with a fully qualified name                             $e9
   for method names                                          $e10
   for attribute names                                       $e11
                                                             """

  def e1 = s2""" code: ${ snippet { 1 + 1 } } """.texts(1).t.trim ===
    """|```
       |1 + 1
       |```""".stripMargin


  def e2 = s2""" code: ${ snippet {
var n = 0
n = 1
} } """.texts(1).t.trim ===
  """|```
     |var n = 0
     |n = 1
     |```""".stripMargin

  def e3 = s2""" code: ${ snippet {
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

  def e4 = s2""" code: ${ snippet {
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

  def e5 = s2""" code: ${ snippet {
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

  def e6 = s2""" code: ${ snippet {
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


  def e7 = s2""" code: ${ snippet {
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

  def e8 = {
    "code"                                         || "markdown"                 |>
    s"""the trait `${simpleName[Snippets]}`"""     !! "the trait `Snippets`"     |
    { (code, markdown) => texts(code)(0) === markdown}
  }

  def e9 = {
    "code"                                   || "markdown"                                      |>
    s"""the trait `${fullName[Snippets]}`""" !! "the trait `org.specs2.specification.Snippets`" |
    { (code, markdown) => texts(code)(0) === markdown}
  }

  def e10 = {
    "code"                              || "markdown"                                      |>
    s"""the method `${termName(is)}`""" !! "the method `is`"                               |
      { (code, markdown) => texts(code)(0) === markdown}
  }

  def e11 = {
    "code"                                         || "markdown"                  |>
    s"""the attribute `${termName(attribute1)}`""" !! "the attribute `attribute1`" |
      { (code, markdown) => texts(code)(0) === markdown}
  }
  val attribute1 = 1


  def texts(fs: Fragments) = fs.texts.map(_.t).toIndexedSeq
}

