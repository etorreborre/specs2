package org.specs2
package main

class SystemPropertiesSpec extends Specification { def is = s2"""

Set
===

 the getOrElse(name, defaultValue) method returns
   the value of the 'specs2.name' property if found $get1
     even if capitalized differently $get2
   the value of the 'name' property if found $get3
   the default value if not found $get4

Get if
======

 the getIf(name, value) method returns
   Some(value) if the property is found $getIf1
   None if the property is not found $getIf2

 the getIfElse(name, v1)(v2) method returns
   v1 if the property is found $getIfElse1
   v2 the property is not found $getIfElse2

Get as
======

 the getPropertyAs method returns
   None if the property is declared with just no value $getAs1
   Some(true) if the property is declared as 'true' $getAs2
   Some(false) if the property is declared as 'false' $getAs3
   None if the property is not declared $getAs4
"""

  val dirs = Map("specs2.outdir" -> "target/results")

  def get1 = sp(dirs).getOrElse("outdir", "") === "target/results"
  def get2 = sp(dirs).getOrElse("outDir", "") === "target/results"
  def get3 = sp(dirs).getOrElse("specs2.outdir", "") === "target/results"
  def get4 = sp(dirs).getOrElse("specs2.missing", "default") === "default"

  val colors = Map("specs2.whitebg" -> "")

  def getIf1 = sp(colors).getIf("whitebg", 1) must beSome(1)
  def getIf2 = sp(colors).getIf("whitebgxxx", 1) must beNone
  def getIfElse1 = sp(colors).getIfElse("whitebg", 1)(2) must_== 1
  def getIfElse2 = sp(colors).getIfElse("whitebgxxx", 1)(2) must_== 2

  case class props(properties: (String, String)*) extends SystemProperties:
    override def systemGetProperty(p: String) = Map(properties: _*).get(p)

  def getAs1 =
    props("specs2.color" -> null).getPropertyAs[Boolean]("color") must beNone
  def getAs2 =
    props("specs2.color" -> "true").getPropertyAs[Boolean]("color") must beSome(
      true
    )
  def getAs3 =
    props("specs2.color" -> "false")
      .getPropertyAs[Boolean]("color") must beSome(false)
  def getAs4 =
    props("specs2.other" -> "false").getPropertyAs[Boolean]("color") must beNone
}

/**
  * This class is used in specifications to mock the system properties
  */
case class sp(properties: Map[String, String]) extends SystemProperties:
  override def systemGetProperty(p: String) = properties.get(p)
