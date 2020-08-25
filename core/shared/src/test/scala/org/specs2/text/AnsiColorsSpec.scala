package org.specs2
package text

import specification.Tables
import AnsiColors._

class AnsiColorsSpec extends Spec with Tables { def is = s2"""

 it is possible to remove the colors from a string $colors1
 coloring a string must keep newlines              $colors2

"""

  def colors1 =
    removeColors("hello" + AnsiColors.red.color) must_== "hello"

  def colors2 =
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
