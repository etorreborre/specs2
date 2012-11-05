package org.specs2
package control

import org.specs2.Specification
import specification.Grouped
import Stacktraces._

class StacktracesSpec extends Specification with Grouped { def is =

  "it is possible to if a piece of code is executed from a given library by inspecting the stacktrace" ! g1.e1

  "executed" - new g1 {
    e1 := "this code is executed from specs2" ==> { isExecutedFrom("specs2") === true }
  }


}
