package org.specs2
package control

import Stacktraces._

class StacktracesSpec extends Specification { def is = s2"""

  it is possible to if a piece of code is executed from a given library by inspecting the stacktrace $fromLibrary

"""

  def fromLibrary = "this code is executed from specs2" ==> { isExecutedFrom("specs2") === true }

}
