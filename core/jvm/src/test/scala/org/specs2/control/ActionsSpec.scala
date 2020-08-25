package org.specs2.control

import org.specs2._
import io._

class ActionsSpec extends Specification { def is = s2"""

  guarding an operation throwing an exception $guard

"""

  def guard =
    val operation = FileSystem(NoLogger).readFile(FilePath("missing")).orElse(Operation.ok("ok"))
    operation.runOption === Some("ok")

}
