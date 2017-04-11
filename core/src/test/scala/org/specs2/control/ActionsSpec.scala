package org.specs2.control

import org.specs2._
import io._
import eff._, syntax.all._

class ActionsSpec extends Specification { def is = s2"""

  guarding an action throwing an exception $guard

"""

  def guard = {
    val operation = FileSystem.readFile(FilePath("missing")) orElse warnAndFail[OperationStack, String]("warning here", "failed")
    val (value, warnings) = executeOperation(operation)
    (value must beLeft) and (warnings must haveSize(1))
  }

}
