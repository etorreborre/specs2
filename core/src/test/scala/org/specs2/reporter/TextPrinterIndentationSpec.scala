package org.specs2
package reporter

import main.Arguments
import org.specs2.specification.core._
import control._

class TextPrinterIndentationSpec(env: Env) extends Specification { def is = s2"""

 The default indentation for the text printer is 2
 ${ TextPrinter.indentationSize(Arguments("")) === 2 }

 But this can be set from the command-line
 ${ TextPrinter.indentationSize(Arguments("indentation", "4")) === 4 }

 The specified indentation defines the number of spaces to use to indent text $indentationSpaces
"""

  def indentationSpaces = {
    val logger = LineLogger.stringLogger
    val spec: SpecStructure =
s2"""
text$t
other text
"""
    TextPrinter.print(env.setArguments(Arguments("indentation", "4")).setLineLogger(logger))(spec).runOption(env.executionEnv)
    (logger.messages(1), logger.messages(2)) must_==
      (("[info] text    ",
        "[info]     other text"))
  }

}
