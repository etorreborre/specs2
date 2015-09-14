package org.specs2
package scalacheck

import main._

class CommandLinePropertiesMutableSpec extends mutable.Specification with ScalaCheck {
  "use the command line to drive properties" >> {
    val specification = new UserSpecification
    specs2.run(specification)(Arguments("scalacheck.mintestsok", "50"))
    specification.i === 50
  }
}

class UserSpecification extends mutable.Specification with ScalaCheck {
  var i = 0
  "use the command line to drive properties" >> prop { n: Int =>
    // test i
    i += 1
    ok
  }
}
