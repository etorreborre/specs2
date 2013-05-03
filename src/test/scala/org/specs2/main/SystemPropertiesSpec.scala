package org.specs2
package main

import specification._

class SystemPropertiesSpec extends script.Specification with Groups { def is = s2"""

 the getOrElse(name, defaultValue) method returns
   + the value of the 'specs2.name' property if found
     + even if capitalized differently
   + the value of the 'name' property if found
   + the default value if not found

 the getIf(name, value) method returns
   + Some(value) if the property is found
   + None if the property is not found

 the getIfElse(name, v1)(v2) method returns
   + v1 if the property is found
   + v2 the property is not found

 the getPropertyAs method returns
   + None if the property is declared with just no value
   + Some(true) if the property is declared as 'true'
   + Some(false) if the property is declared as 'false'
   + None if the property is not declared
                                                                                             """

  "set" - new g1 with SystemProperties {
    override lazy val properties = Map("specs2.outdir" -> "target/results")

    e1 := getOrElse("outdir", "")                === "target/results"
    e2 := getOrElse("outDir", "")                === "target/results"
    e3 := getOrElse("specs2.outdir", "")         === "target/results"
    e4 := getOrElse("specs2.missing", "default") === "default"
  }

  "getIf" - new g2 with SystemProperties {
    override lazy val properties = Map("specs2.whitebg" -> "")

    e1 := getIf("whitebg", 1)           must beSome(1)
    e2 := getIf("whitebgxxx", 1)        must beNone
    e3 := getIfElse("whitebg", 1)(2)    must_== 1
    e4 := getIfElse("whitebgxxx", 1)(2) must_== 2
  }

  "getAs" - new g3 {
    case class props(map:(String, String)*) extends SystemProperties {
      override lazy val properties = Map(map:_*)
    }
    e1 := props("specs2.color" -> null).getPropertyAs[Boolean]("color")    must beNone
    e2 := props("specs2.color" -> "true").getPropertyAs[Boolean]("color")  must beSome(true)
    e3 := props("specs2.color" -> "false").getPropertyAs[Boolean]("color") must beSome(false)
    e4 := props("specs2.other" -> "false").getPropertyAs[Boolean]("color") must beNone
  }
}