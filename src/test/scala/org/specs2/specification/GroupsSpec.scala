package org.specs2
package specification

import org.specs2.mutable.Specification
import execute.AsResult

class GroupsSpec extends Specification {

  "The examples of a group must only be evaluated when the example is evaluated" >> {
    val g = new Group {}
    var evaluated = false
    g.e1 := { evaluated = true; ok.pp }

    "the example is not evaluated" ==> { evaluated === false }

    "evaluate example" ==> AsResult(g.e1)
    "the example is evaluated" ==> { evaluated === true }
  }

}
