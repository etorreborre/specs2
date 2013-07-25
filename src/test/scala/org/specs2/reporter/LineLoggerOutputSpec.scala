package org.specs2
package reporter

import specification._
import org.specs2.io.StringOutput
import org.specs2.main.Arguments

class LineLoggerOutputSpec extends script.Specification with Groups { def is = s2"""

 A LineLoggerOutput prints the results of an executed specification to a LineLogger, that is an object which only
 prints info, error and failures line by line. Thus the LineLoggerOutput must be very careful in managing the newlines
 when it is passed arbitrary text.

Simple cases
============

 + one example
 + one example and a total
 + one example and a pending
"""

  "simple cases" - new group with lineLogger {
    implicit lazy val arguments = Arguments("nocolor")

    eg := {
      printed {
        printText("some text\n  ")
        printSuccess("+ one example")
      } must contain(exactly("some text\n  + one example"))
    }

    eg := {
      printed {
        printText("some text\n  ")
        printSuccess("+ one example")
        printText("\n")
        printStats("Total for specification spec1")
        printStats(Stats().display)
        printLine("")
      } must contain(allOf("some text\n  + one example\nTotal for specification spec1",
                           "Finished in 0 ms\n0 example, 0 failure, 0 error", ""))
    }

    eg := {
      printed {
        printSuccess("+ one example")
        printText("\n")
        printPending("* pending")
      } must contain(exactly("+ one example\n* pending"))
    }
  }

  trait lineLogger extends LineLoggerOutput with StringOutput {
    def infoLog(msg: String)    = append(msg)
    def errorLog(msg: String)   = append(msg)
    def failureLog(msg: String) = append(msg)

    def printed(actions: =>Any)(implicit args: Arguments) = {
      actions
      super.flushText(force = true)
      val m = messages
      clear
      m
    }
  }
}
