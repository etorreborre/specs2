package org.specs2
package text

import org.specs2.specification.{Tables, Grouped}
import AnsiColors._

class AnsiColorsSpec extends Spec with Grouped with Tables { def is = s2"""

 it is possible to remove the colors from a string                                                 ${g1.e1}
 coloring a string must keep newlines                                                              ${g1.e2}
                                                                                                   """

  new g1 {
    e1 := removeColors("hello" + AnsiColors.red.color) must_== "hello"

    e2 := {
      val ^ = reset.color
      "string to color"        | "result"                                  |>
      "\nhello\n"              ! s"*${^}\n*hello${^}\n*${^}"               |
      "\nhello"                ! s"*${^}\n*hello${^}"                      |
      "hello\n"                ! s"*hello${^}\n*${^}"                      |
      "hello"                  ! s"*hello${^}"                             |
      ""                       ! s"*${^}"                                  |
      "\nhello\nworld\n"       ! s"*${^}\n*hello${^}\n*world${^}\n*${^}"   |
      { (s, r) => color(s, AnsiColor("*")).replace("\n", "_").replace(^, "^") must_==  r.replace("\n", "_").replace(^, "^") }
    }
  }

}