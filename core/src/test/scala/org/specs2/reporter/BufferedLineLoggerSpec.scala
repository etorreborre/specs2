package org.specs2
package reporter

import LineLogger._

class BufferedLineLoggerSpec extends Spec { def is = s2"""

 Newlines must be buffered $a1

"""

  def a1 = {
    val logger = stringLogger
    logger.infoLog("Hello world\n")
    logger.infoLog("How are you?")
    logger.close

    logger.messages must_== Seq("[info] Hello world", "[info] How are you?")
  }
}
