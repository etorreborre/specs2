package org.specs2
package text

import matcher._

class TextTableSpec extends mutable.Spec with TypedEqual:
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
  "a text table must allow results to span several lines" >> {
    "\n"+TextTable(
      Seq("",   "a"    , "b",    "c"),
      Seq("+", "hello", "you",  "hello\nyou"),
      Seq("+", "you",   "hello", "you\nhello"),
      Seq("+", "y",     "h",     "y h"      )).show.replace(" ", "_") ===
      s"""|
          |  | a     | b     | c____
          |+ | hello | you   | hello
          |  |       |       | you__
          |+ | you   | hello | you__
          |  |       |       | hello
          |+ | y     | h     | y h  """.stripMargin.replace(" ", "_")
  }
