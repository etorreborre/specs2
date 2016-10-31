package org.specs2.control

import org.specs2._
import io._
import eff._, syntax.all._
import org.specs2.matcher.DisjunctionMatchers

class ActionsSpec extends Specification with DisjunctionMatchers { def is = s2"""

  guarding an action throwing an exception $guard

"""

  def guard = {
    val operation = FileSystem.readFile(FilePath("missing")) orElse warnAndFail[OperationStack, String]("warning here", "failed")
    val (value, warnings) = executeOperation(operation)
    (value must be_-\/) and (warnings must haveSize(1))
  }

}
