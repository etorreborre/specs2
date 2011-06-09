package org.specs2
package main

class SystemPropertiesSpec extends Specification { def is =

  "the getOrElse(name, defaultValue) method returns"                                            ^
    "the value of the 'specs2.name' property if found"                                          ! set().e1 ^
      "even if capitalized differently"                                                         ! set().e1_1 ^ bt^
    "the value of the 'name' property if found"                                                 ! set().e2 ^
    "the default value if not found"                                                            ! set().e3 ^
                                                                                                end

  case class set() extends SystemProperties {
    override lazy val properties = Map("specs2.outdir" -> "target/results")

    def e1 = getOrElse("outdir", "") must_== "target/results"
    def e1_1 = getOrElse("outDir", "") must_== "target/results"
    def e2 = getOrElse("specs2.outdir", "") must_== "target/results"
    def e3 = getOrElse("specs2.missing", "default") must_== "default"
  }

}