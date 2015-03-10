package org.specs2
package text

class IndentSpec extends Specification { def is = s2"""

  lastLineIndentation returns the spaces before the last line                     $lastIndentation
  indentAllButFirstLine indents all the lines of some text but not the first one  $indentAllButFirst
"""

  def lastIndentation = Indent.lastLineIndentation(
    """|Some text
       | with some indentation on the
       |    last line  """.stripMargin) === "    "


  def indentAllButFirst = Indent.indentAllButFirstLine(
    """|This is some
       | text
       |to
       |  be indented""".stripMargin, "--") ===
    """|This is some
       |-- text
       |--to
       |--  be indented""".stripMargin

}
