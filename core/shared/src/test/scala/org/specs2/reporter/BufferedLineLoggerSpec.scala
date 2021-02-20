package org.specs2
package reporter

import PrinterLogger.*

class BufferedPrinterLoggerSpec extends Spec { def is = s2"""

 Newlines must be buffered $a1

"""

  def a1 =
    val logger = stringPrinterLogger
    logger.infoLog("Hello world\n")
    logger.infoLog("How are you?")
    logger.close()

    logger.messages `must` ===(Seq("[info] Hello world", "[info] How are you?"))
}
