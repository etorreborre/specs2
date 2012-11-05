package org.specs2
package text

import specification.Grouped
import AnsiColors._

class AnsiColorsSpec extends Specification with Grouped { def is =

  "it is possible to remove the colors from a string"                                                                   ! g1.e1^
  "coloring a string must keep newlines"                                                                                ^
	  "if start and end with newline"                                                                                     ! g1.e2^
	  "if start with newline"                                                                                             ! g1.e3^
	  "if end with newline"                                                                                               ! g1.e4^
	  "if no newline"                                                                                                     ! g1.e5^
	  "if empty"                                                                                                          ! g1.e6^
    "if multiline"                                                                                                      ! g1.e7^
                                                                                                                        end

  new g1 {
    e1 := removeColors("hello" + AnsiColors.red) === "hello"

    e2 := color("\nhello\n", "*")        === "\n*hello"+reset+"\n"
    e3 := color("\nhello", "*")          === "\n*hello"+reset
    e4 := color("hello\n", "*")          === "*hello"+reset+"\n"
    e5 := color("hello", "*")            === "*hello"+reset
    e6 := color("", "*")                 === ""
    e7 := color("\nhello\nworld\n", "*") === "\n*hello"+reset+"\n*world"+reset+"\n"
  }

}