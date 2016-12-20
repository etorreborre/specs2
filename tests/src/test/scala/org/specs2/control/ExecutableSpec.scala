package org.specs2
package control

import io._
import matcher._

class ExecutableSpec extends Specification with OperationMatchers { def is = section ("travis") ^ s2"""

 we can use the Executable object to get git tags $tags

"""

  def tags = {
    Executable.execute(FilePath("git"), Seq("tag")) must beOk((_: String) must not(beEmpty))
  }
}
