package org.specs2
package control

import org.specs2.Specification
import specification._
import Stacktraces._

class StacktracesSpec extends script.Specification with Grouped { def is = s2"""

  + it is possible to if a piece of code is executed from a given library by inspecting the stacktrace

  """

  "executed" - new group {
    eg := "this code is executed from specs2" ==> { isExecutedFrom("specs2") === true }
  }


}
