package org.specs2
package main
import specification._
import matcher.ThrownExpectations

class SystemPropertiesSpec extends Specification with ThrownExpectations { def is =

  "if a specs2 property is set, it is returned" ! set().e1 ^
                                                end

  case class set() extends SystemProperties {
    override lazy val properties = Map("specs2.outDir" -> "target/results")

    def e1 = getOrElse("outDir", "") must_== "target/results"
  }

}