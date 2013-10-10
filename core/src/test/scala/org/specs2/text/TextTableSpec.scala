package org.specs2
package text


class TextTableSpec extends mutable.Specification {
  "a text table must format it content to equal length" >> {
    TextTable(
      Seq("",   "a"    , "b",    "c"),
      Seq("+", "hello", "you",  "hello you"),
      Seq("+", "you",   "hello", "you hello"),
      Seq("+", "y",     "h",     "y h"      )).show ===
      "  | a     | b     | c        "+"\n"+
      "+ | hello | you   | hello you"+"\n"+
      "+ | you   | hello | you hello"+"\n"+
      "+ | y     | h     | y h      "


  }
}