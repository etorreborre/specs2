package org.specs2
package main

class SystemPropertiesSpec extends Specification { def is =

  "the getOrElse(name, defaultValue) method returns"                                            ^
    "the value of the 'specs2.name' property if found"                                          ! set().e1 ^
      "even if capitalized differently"                                                         ! set().e2 ^ bt^
    "the value of the 'name' property if found"                                                 ! set().e3 ^
    "the default value if not found"                                                            ! set().e4 ^
                                                                                                p^
  "the getIf(name, value) method returns"                                                       ^
    "Some(value) if the property is found"                                                      ! getIf().e1 ^
    "None if the property is not found"                                                         ! getIf().e2 ^
                                                                                                p^
  "the getIfElse(name, v1)(v2) method returns"                                                  ^
    "v1 if the property is found"                                                               ! getIf().e3 ^
    "v2 the property is not found"                                                              ! getIf().e4 ^
                                                                                                end

  case class set() extends SystemProperties {
    override lazy val properties = Map("specs2.outdir" -> "target/results")

    def e1 = getOrElse("outdir", "") must_== "target/results"
    def e2 = getOrElse("outDir", "") must_== "target/results"
    def e3 = getOrElse("specs2.outdir", "") must_== "target/results"
    def e4 = getOrElse("specs2.missing", "default") must_== "default"
  }

  case class getIf() extends SystemProperties {
    override lazy val properties = Map("specs2.whitebg" -> "")

    def e1 = getIf("whitebg", 1) must beSome(1)
    def e2 = getIf("whitebgxxx", 1) must beNone
    def e3 = getIfElse("whitebg", 1)(2) must_== 1
    def e4 = getIfElse("whitebgxxx", 1)(2) must_== 2
  }

}