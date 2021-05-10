package org.specs2
package scalacheck

import main.*

class CommandLinePropertiesMutableSpec extends mutable.Specification with ScalaCheck:
  "use the command line to drive properties" >> {
    val specification = new UserSpecification
    given Arguments = Arguments("scalacheck.mintestsok", "50", "junit")
    specs2.run.apply(specification)
    specification.i === 50
  }

class UserSpecification extends mutable.Specification with ScalaCheck:
  var i = 0
  "use the command line to drive properties" >> prop { (n: Int) =>
    // test i
    i += 1
    ok
  }
