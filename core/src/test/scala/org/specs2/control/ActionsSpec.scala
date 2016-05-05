package org.specs2.control

import org.specs2._
import io._
import eff._, syntax.all._
import org.specs2.matcher.DisjunctionMatchers

class ActionsSpec extends Specification with DisjunctionMatchers { def is = s2"""

  guarding an action throwing an exception $guard

"""

  def guard = {
    val action = FileSystem.readFile(FilePath("missing")) orElse warnAndFail[ActionStack, String]("warning here", "failed")
    val (value, warnings) = executeAction(action)
    (value must be_-\/) and (warnings must haveSize(1))
  }

}
