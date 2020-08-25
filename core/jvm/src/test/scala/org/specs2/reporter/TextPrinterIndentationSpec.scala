package org.specs2
package reporter

import main.Arguments
import org.specs2.specification.core._

class TextPrinterIndentationSpec(val env: Env) extends Specification with OwnEnv { def is = s2"""

 The default indentation for the text printer is 2
 ${ TextPrinter.default.indentationSize(Arguments("")) === 2 }

 But this can be set from the command-line
 ${ TextPrinter.default.indentationSize(Arguments("indentation", "4")) === 4 }

 The specified indentation defines the number of spaces to use to indent text $indentationSpaces
"""

  def indentationSpaces =
    val logger = PrinterLogger.stringPrinterLogger
    val spec: SpecStructure =
      s2"""
      |text$t
      |other text
      |""".stripMargin
      
    val printer = TextPrinter(ownEnv.setArguments(Arguments("indentation", "4")).setPrinterLogger(logger))
    printer.print(spec).runOption(ownEnv.executionEnv)
    (logger.messages(1), logger.messages(2)) must_==
      (("[info] text    ",
        "[info]     other text"))

}
