package org.specs2
package specification

import matcher.*
import execute.Snippet.*
import core.*

// format: off
class SnippetsSpec(val env: Env)
    extends Specification
    with Snippets
    with DataTables
    with TypedEqual
    with OwnExecutionEnv {
  def is = s2"""

 These are examples on how to use the various snippet methods

  with the `snippet` method $snippet1
  with the `snippet` method and 2 lines $snippet2
  with the `snippet` method and cut comments $snippet3
  with the `snippet` method and cut comments - 2 blocks $snippet4
  with some code having accolades $snippet5

Offsets
=======
 It is possible to specify an offset to the snippet
   with the `snippet` method
    a positive offset $offset1
    a negative offset $offset2

Trimming
========
 An approximated expression must
   not include the `snippet` call $offset3
   not have parameter setting calls $offset4

Results
=======
 Results can be displayed by using the `eval` method
  when using the `snippet` method $results1

Names
=====
 It is also possible to capture code names
   for types (trait, classes,...) $names1
     with a fully qualified name $names2
   for method names $names3
   for method names with type parameters $names4
   for attribute names $names5

Robustness
==========
  A snippet must not fail if the code throws an exception $effects1
  An interpolated snippet code must not be executed $effects2

"""

  def snippet1 =
    s2""" code: ${snippet{got {1 + 1}}} """.trimmedTexts(1) === "`got {1 + 1}`"

  def snippet2 =
    s2""" code: ${snippet{
got {
  var n = 0
  n = 1
}
  }}""".trimmedTexts(1) ===
    """|```
       |got {
       |  var n = 0
       |  n = 1
       |}
       |```""".stripMargin

  def snippet3 = s2""" code: ${snippet{
// 8<--
var n = 0
// 8<--
n = 1
// 8<--
n = 0
  }}""".trimmedTexts(1).trim ===
    """|```
       |n = 1
       |```""".stripMargin

  def snippet4 = s2""" code: ${snippet{
// 8<--
var n = 0
// 8<--
n = 1
// 8<--
n = 0
// 8<--
var i = 0
i = 1
  }}""".trimmedTexts(1) ===
    """```
      |n = 1
      |var i = 0
      |i = 1
      |```""".stripMargin

  def snippet5 =
    s2""" code ${snippet{ "e1" ! {ok} /**/; 1 /**/ }}""".trimmedTexts(1) === """`"e1" ! {ok} /**/; 1 /**/`"""

  def offset1 = s2""" code: ${snippet{
// 8<--
var n = 0
// 8<--
n = 1
// 8<--
n = 0
  }.offsetIs(2)}""".trimmedTexts(1) ===
    """|```
       |  n = 1
       |```""".stripMargin

  def offset2 = s2""" code: ${snippet{
    // 8<--
  var n = 0
    // 8<--
  n = 1
    // 8<--
  n = 0
  }.offsetIs(-2)}""".trimmedTexts(1) ===
    """|```
       |n = 1
       |```""".stripMargin

  def offset3 =
    "code" || "result" |>
      "snippet{ hello }" !! "hello" |
      " snippet{ hello }" !! "hello" |
      " snippet { hello }" !! "hello" |
      " snippet{ hello } " !! "hello" |
      " snippet{\n hello \n} " !! "hello" |
      " snippet{ hello \n} " !! "hello" | { (c, r) => trimApproximatedSnippet(c) === r }

  def offset4 =
    "code" || "result" |>
      " snippet{ hello \n}.set(eval = true) " !! "hello" |
      " snippet{ hello \n}.eval " !! "hello" |
      " snippet{ hello \n}.offsetIs(2) " !! "hello" | { (c, r) => trimApproximatedSnippet(c) === r }

  def results1 = s2""" code: ${snippet{
  var n = 1
  n = 1 + n
  n
  }.eval.offsetIs(-2)}""".trimmedTexts.drop(1).take(3).mkString("\n") ===
    """|```
       |var n = 1
       |n = 1 + n
       |n
       |```
       |`> 2`""".stripMargin

  def names1 =
    "code" || "markdown" |>
      s"""the trait `${simpleName[Snippets]}`""" !! "the trait `Snippets`" | { (code, markdown) => code === markdown }

  def names2 =
    "code" || "markdown" |>
      s"""the trait `${fullName[Snippets]}`""" !! "the trait `org.specs2.specification.Snippets`" | {
        (code, markdown) => code === markdown
      }

  def names3 =
    "code" || "markdown" |>
      s"""the method `${termName(is)}`""" !! "the method `is`" | { (code, markdown) => code === markdown }

  def names4 =
    def function[T, S](t: T, s: S) = ""

    "code" || "markdown" |>
      s"""the method `${termName(function(1, ""))}`""" !! "the method `function`" |
      s"""the method `${termName(function[Int, String](1, ""))}`""" !! "the method `function`" | { (code, markdown) =>
        code === markdown
      }

  def names5 =
    "code" || "markdown" |>
      s"""the attribute `${termName(attribute1)}`""" !! "the attribute `attribute1`" | { (code, markdown) =>
        code === markdown
      }

  def effects1 =
    snippet[Unit](sys.error("boom")) must not(throwAn[Exception])

  def effects2 =
    var i = 0
    s2""" start ${snippet{ i = 1; i }} end """
    i === 0

  // HELPERS

  def got[T](t: T) = t

  extension (fs: Fragments) def trimmedTexts = fs.fragmentsList(ee).filter(Fragment.isText).map(_.description.show.trim)

  val attribute1 = 1
}
