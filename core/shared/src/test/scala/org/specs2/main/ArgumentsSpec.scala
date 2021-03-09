package org.specs2
package main

import matcher.{TypedEqual, DataTables}
import execute.Result
import text.MappedColors
import control.*

class ArgumentsSpec extends Spec with DataTables with TypedEqual with ArgProperties { def is = s2"""

Arguments can be passed on the command line as an Array of Strings. There are 2 types of arguments:

 * boolean arguments which only presence means that their value is true
   e.g. `xonly` to show only failures and errors

 * string arguments which have a specific value
   e.g. `srcTestDir src/test` to specify the directory holding the source files

Definition
==========

  If an argument is specified, its value is returned
    for a boolean argument like xonly the value is true                        $values1
    a boolean argument can be negated by adding ! in front of it.              $values2
      ex: `Arguments("!pandoc").commandLine.boolOr("pandoc", true) is false
    for a string argument, it is the 'next' value                              $values3

  If an argument is not specified, its default value is returned
    for a boolean argument like xonly, it is false                             $values4
    for a string argument like ex, it is .*                                    $values5

  The argument names can be capitalized or no
    for a boolean argument like xonly, xOnly is admissible                     $values6
    for a string argument like colorsclass, colorsClass is admissible          $values7
    but the name has to match exactly, 'exclude' must not be mistaken for 'ex' $values8

    Quoted arguments must be been properly passed                              $values9
    Some boolean arguments have negated names, like nocolor, meaning !color    $values10

Overriding
==========

  An Arguments instance can be overridden by another with the `<|` operator: `a <| b`
    if there's no corresponding value in b, the value in a stays                        $overriding1
    there is a corresponding value in b, the value in a is overridden when there is one $overriding2
    there is a corresponding value in b, the value in b is kept                         $overriding3

System props
============

  Arguments can also be passed from system properties
    a boolean value just have to exist as -Dname                                            $properties1
    a boolean value can be -Dname=true                                                      $properties2
    a boolean value can be -Dname=false                                                     $properties3
    a string value will be -Dname=value                                                     $properties4
    properties can also be passed as -Dspecs2.name to avoid conflicts with other properties $properties5
    with the color/nocolor property                                                         $properties6

Execution
=========

  Arguments can decide if a result must be shown or not, depending on its status
    xonly => canShow(x)                               $execution1
    xonly => canShow(result.status)                   $execution2

  Some values can be filtered from the command line
    to include only some arguments                    $execution3
    to exclude some arguments                         $execution4

Creation
========

  Arguments can be created from a sequence of strings
    to declare a Notifier                             $creation1

"""

  def values1 = Arguments("xonly").xonly must beTrue
  def values2 = Arguments("!pandoc").commandLine.boolOr("pandoc", true) must beFalse
  def values3 = Arguments("ex", "Hello").ex must ===(".*Hello.*")

  def values4 = Arguments("").xonly must beFalse
  def values5 = Arguments("").ex must ===(".*")

  def values6 = Arguments("xOnly").xonly must beTrue
  def values7 = Arguments("colorClass", classOf[MappedColors].getName).colors must ===(MappedColors())
  def values8 = Arguments("exclude", "spec").ex must ===(Arguments().ex)

  def values9 = Arguments("ex", "this test").select.ex must ===(".*this test.*")

  def values10 =
    List("nocolor", "color", "nocolor true", "nocolor false", "color true", "color false").map(a => Arguments.split(a).color) must ===(
    List(false, true, false, true, true, false))

  def overriding1 = (args(xonly = true) <| args(plan = false)).xonly must ===(true)
  def overriding2 = args(xonly = true).overrideWith(args(xonly = false)).xonly must ===(false)
  def overriding3 = (args(xonly = true) <| args(plan = true)).plan must ===(true)

  case class properties(properties: (String, String)*) extends SystemProperties:
    override def systemGetProperty(p: String) = Map(properties*).get(p)

  def properties1 = Arguments.extract(using   Seq(""), properties("plan" -> "")).plan must ===(true)
  def properties2 = Arguments.extract(using   Seq(""), properties("plan" -> "true")).plan must ===(true)
  def properties3 = Arguments.extract(using   Seq(""), properties("plan" -> "false")).plan must ===(false)
  def properties4 = Arguments.extract(using   Seq(""), properties("ex"   -> "spec")).ex must ===(".*spec.*")
  def properties5 = Arguments.extract(using   Seq(""), properties("specs2.ex" -> "spec")).ex must ===(".*spec.*")

  def properties6 =
    List(("nocolor", ""), ("color", ""), ("nocolor", "true"), ("nocolor", "false"), ("color", "true"), ("color", "false")).map { case (k, v) =>
      Arguments.extract(using Seq(""), properties(k -> v)).color
    } must ===(List(false, true, false, true, true, false))

  def execution1 =
    "args"                      | "status" | "canShow"    |>
    xonly                       ! "x"      ! true         |
    xonly                       ! "!"      ! true         |
    xonly                       ! "o"      ! false        |
    xonly                       ! "+"      ! false        |
    xonly                       ! "-"      ! false        |
    showOnly("x!")              ! "x"      ! true         |
    showOnly("x!")              ! "!"      ! true         |
    showOnly("x!")              ! "o"      ! false        |
    showOnly("x!")              ! "+"      ! false        |
    showOnly("x!")              ! "-"      ! false        |
    showOnly("o")               ! "x"      ! false        |
    showOnly("o")               ! "!"      ! false        |
    showOnly("o")               ! "o"      ! true         |
    showOnly("o")               ! "+"      ! false        |
    showOnly("o")               ! "-"      ! false        |
    Arguments("showonly","o")   ! "x"      ! false        |
    Arguments("showonly","o")   ! "!"      ! false        |
    Arguments("showonly","o")   ! "o"      ! true         |
    Arguments("showonly","o")   ! "+"      ! false        |
    Arguments("showonly","o")   ! "-"      ! false        |
    { (a, s, r) =>  a.canShow(s) must ===(r) }

  def execution2 =
    "args"                     | "status"            | "canShow"    |>
    xonly                      ! (failure:Result)    ! true         |
    xonly                      ! anError             ! true         |
    xonly                      ! skipped             ! false        |
    xonly                      ! success             ! false        |
    showOnly("x!")             ! failure             ! true         |
    showOnly("x!")             ! anError             ! true         |
    showOnly("x!")             ! skipped             ! false        |
    showOnly("x!")             ! success             ! false        |
    showOnly("o")              ! failure             ! false        |
    showOnly("o")              ! anError             ! false        |
    showOnly("o")              ! skipped             ! true         |
    showOnly("o")              ! success             ! false        |
    { (a, s, r) =>  a.canShow(s.status) must ===(r) }

  def execution3 =
    Arguments("this", "is", "cool").commandLineFilter("this", "cool").commandLine.arguments === Seq("this", "cool")

  def execution4 =
    Arguments("this", "is", "cool").commandLineFilterNot("this", "cool").commandLine.arguments === Seq("is")

  def creation1 =
    Arguments("MySpec", "notifier", "IntelliJNotifier").report.notifier === "IntelliJNotifier"
}
