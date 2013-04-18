package org.specs2
package specification

import matcher.DataTables

class SnippetsSpec extends Specification with Snippets with DataTables with Grouped { def is = s2"""

 These are examples on how to use the various snippet methods

   with the `snippet` method                                 ${g1.e1}
   with the `snippet` method and 2 lines                     ${g1.e2}
   with the `snippet` method and cut comments                ${g1.e3}
   with the `snippet` method and cut comments - 2 blocks     ${g1.e4}

 It is possible to specify an offset to the snippet
   with the `snippet` method
     a positive offset                                       ${g2.e1}
     a negative offset                                       ${g2.e2}

 Results can be displayed by using the `eval` method
   when using the `snippet` method                           ${g3.e1}

 It is also possible to capture code names
   for types (trait, classes,...)                            ${g4.e1}
     with a fully qualified name                             ${g4.e2}
   for method names                                          ${g4.e3}
   for method names with type parameters                     ${g4.e4}
   for attribute names                                       ${g4.e5}

 A snippet must not fail if the code throws an exception     ${g5.e1}
 An interpolated snippet code must not be executed           ${g5.e2}
                                                             """
 "snippets capture" - new g1 {

  e1 := { s2""" code: ${ snippet { got {1 + 1} } } """.texts(1).t.trim === "`got {1 + 1}`" }
  def got[T](t: T) = t


  e2 := s2""" code: ${ snippet {
got {
  var n = 0
  n = 1
}
} } """.texts(1).t.trim ===
    """|```
       |got {
       |  var n = 0
       |  n = 1
       |}
       |```""".stripMargin

  e3 := s2""" code: ${ snippet {
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

  e4 := s2""" code: ${ snippet {
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
  }

  "offsets" - new g2 {
    e1 := s2""" code: ${ snippet {
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

    e2 := s2""" code: ${ snippet {
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

  }

  "results" - new g3 {
    e1 := s2""" code: ${ snippet {
  var n = 1
  1 + n
  }.eval.offsetIs(-2) }""".texts.drop(1).take(2).map(_.t.trim).mkString("\n") ===
    """|```
       |var n = 1
       |1 + n
       |```
       |`> 2`""".stripMargin
  }

  "names" - new g4 {

    e1 := {
      "code"                                         || "markdown"                 |>
      s"""the trait `${simpleName[Snippets]}`"""     !! "the trait `Snippets`"     |
      { (code, markdown) => texts(code)(0) === markdown}
    }

    e2 := {
      "code"                                   || "markdown"                                      |>
      s"""the trait `${fullName[Snippets]}`""" !! "the trait `org.specs2.specification.Snippets`" |
      { (code, markdown) => texts(code)(0) === markdown}
    }

    e3 := {
      "code"                              || "markdown"                                      |>
      s"""the method `${termName(is)}`""" !! "the method `is`"                               |
        { (code, markdown) => texts(code)(0) === markdown}
    }

    e4 := {
      def function[T, S](t: T, s: S) = ""
      "code"                                                        || "markdown"                                      |>
      s"""the method `${termName(function(1, ""))}`"""              !! "the method `function`"                         |
      s"""the method `${termName(function[Int, String](1, ""))}`""" !! "the method `function`"                         |
      { (code, markdown) => texts(code)(0) === markdown}
    }

    e5 := {
      "code"                                         || "markdown"                   |>
      s"""the attribute `${termName(attribute1)}`""" !! "the attribute `attribute1`" |
        { (code, markdown) => texts(code)(0) === markdown}
    }
  }

  "effects" - new g5 {
    e1 := snippet[Unit](sys.error("boom")) must not(throwAn[Exception])

    e2 := {
      var i = 0
      s2""" start ${snippet { i = 1; i }} end """
      i === 0
    }
  }

  def texts(fs: Fragments) = fs.texts.map(_.t).toIndexedSeq
  val attribute1 = 1
}

