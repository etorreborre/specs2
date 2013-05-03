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

  "set" - new group with SystemProperties {
    override lazy val properties = Map("specs2.outdir" -> "target/results")

    eg := getOrElse("outdir", "")                === "target/results"
    eg := getOrElse("outDir", "")                === "target/results"
    eg := getOrElse("specs2.outdir", "")         === "target/results"
    eg := getOrElse("specs2.missing", "default") === "default"
  }

  "getIf" - new group with SystemProperties {
    override lazy val properties = Map("specs2.whitebg" -> "")

    eg := getIf("whitebg", 1)           must beSome(1)
    eg := getIf("whitebgxxx", 1)        must beNone
    eg := getIfElse("whitebg", 1)(2)    must_== 1
    eg := getIfElse("whitebgxxx", 1)(2) must_== 2
  }

  "getAs" - new group {
    case class props(map:(String, String)*) extends SystemProperties {
      override lazy val properties = Map(map:_*)
    }
    eg := props("specs2.color" -> null).getPropertyAs[Boolean]("color")    must beNone
    eg := props("specs2.color" -> "true").getPropertyAs[Boolean]("color")  must beSome(true)
    eg := props("specs2.color" -> "false").getPropertyAs[Boolean]("color") must beSome(false)
    eg := props("specs2.other" -> "false").getPropertyAs[Boolean]("color") must beNone
  }
}