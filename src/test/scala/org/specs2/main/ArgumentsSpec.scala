package org.specs2
package main

import specification.After
import matcher.DataTables
import execute.Result

class ArgumentsSpec extends Specification with DataTables { def is =
                                                                                                                        """
Arguments can be passed on the command line as an Array of Strings. There are 2 types of arguments:

 * boolean arguments which only presence means that their value is true
   e.g. `xonly` to show only failures and errors

 * string arguments which have a specific value
   e.g. `srcDir src/test` to specify the directory holding the source files
                                                                                                                        """^
                                                                                                                        p^
  "If an argument is specified, its value is returned"                                                                  ^
    "for a boolean argument like xonly the value is true"                                                               ! e1^
    "for a string argument, it is the 'next' value"                                                                     ! e2^
                                                                                                                        p^
  "If an argument is not specified, its default value is returned"                                                      ^
    "for a boolean argument like xonly, it is false"                                                                    ! e3^
    "for a string argument like specName, it is .*Spec"                                                                 ! e4^
                                                                                                                        p^
  "The argument names can be capitalized or not"                                                                        ^
    "for a boolean argument like xonly, xOnly is admissible"                                                            ! e5^
    "for a string argument like specName, specname is admissible"                                                       ! e6^
    "but the name has to match exactly, 'exclude' must not be mistaken for 'ex'"                                        ! e7^
                                                                                                                        p^
  "Some boolean arguments have negated names, like nocolor, meaning !color"                                             ! e8^
                                                                                                                        p^
  "An Arguments instance can be overriden by another with the `<|` operator: `a <| b`"                                  ^
    "if there's no corresponding value in b, the value in a stays"                                                      ! e9^
    "there is a corresponding value in b, the value in a is overriden when there is one"                                ! e10^
    "there is a corresponding value in b, the value in b is kept"                                                       ! e11^
                                                                                                                        p^
  "Arguments can also be passed from system properties"                                                                 ^
    "a boolean value just have to exist as -Dname"                                                                      ! e12^
    "a boolean value can be -Dname=true"                                                                                ! e13^
    "a boolean value can be -Dname=false"                                                                               ! e14^
    "a string value will be -Dname=value"                                                                               ! e15^
    "properties can also be passed as -Dspecs2.name to avoid conflicts with other properties"                           ! e16^
                                                                                                                        p^
  "Arguments can decide if a result must be shown or not, depending on its status"                                      ^
    "xonly => showStatus(x)"                                                                                            ! e17^
    "xonly => showStatus(result.status)"                                                                                ! e18^
                                                                                                                        end


  def e1 = Arguments("xonly").xonly must beTrue
  def e2 = Arguments("specName", "spec").specName must_== "spec"

  def e3 = Arguments("").xonly must beFalse
  def e4 = Arguments("").specName must_== ".*Spec"

  def e5 = Arguments("xOnly").xonly must beTrue
  def e6 = Arguments("specname", "spec").specName must_== "spec"
  def e7 = Arguments("exclude", "spec").ex must_== Arguments().ex

  def e8 = Arguments("nocolor").color must beFalse

  def e9 = (args(xonly = true) <| args(plan = false)).xonly must_== true
  def e10 = args(xonly = true).overrideWith(args(xonly = false)).xonly must_== false
  def e11 = (args(xonly = true) <| args(plan = true)).plan must_== true

  case class properties(map:(String, String)*) extends SystemProperties {
    override lazy val properties = Map(map:_*)
  }

  def e12 = Arguments.extract(Seq(""), properties("plan" -> "")).plan must_== true
  def e13 = Arguments.extract(Seq(""), properties("plan" -> "true")).plan must_== true
  def e14 = Arguments.extract(Seq(""), properties("plan" -> "false")).plan must_== false
  def e15 = Arguments.extract(Seq(""), properties("specname" -> "spec")).specName must_== "spec"
  def e16 = Arguments.extract(Seq(""), properties("specs2.specname" -> "spec")).specName must_== "spec"

  def e17 = "args"                        | "status" | "showStatus" |>
            xonly                         ! "x"      ! true         |
            xonly                         ! "!"      ! true         |
            xonly                         ! "o"      ! false        |
            xonly                         ! "+"      ! false        |
            xonly                         ! "-"      ! false        |
            onlyStatus("x!")              ! "x"      ! true         |
            onlyStatus("x!")              ! "!"      ! true         |
            onlyStatus("x!")              ! "o"      ! false        |
            onlyStatus("x!")              ! "+"      ! false        |
            onlyStatus("x!")              ! "-"      ! false        |
            onlyStatus("o")               ! "x"      ! false        |
            onlyStatus("o")               ! "!"      ! false        |
            onlyStatus("o")               ! "o"      ! true         |
            onlyStatus("o")               ! "+"      ! false        |
            onlyStatus("o")               ! "-"      ! false        |
            Arguments("onlystatus","o")   ! "x"      ! false        |
            Arguments("onlystatus","o")   ! "!"      ! false        |
            Arguments("onlystatus","o")   ! "o"      ! true         |
            Arguments("onlystatus","o")   ! "+"      ! false        |
            Arguments("onlystatus","o")   ! "-"      ! false        |
            { (a, s, r) =>  a.showStatus(s) must_== r }

  def e18 = "args"                        | "status"            | "showStatus" |>
             xonly                        ! (failure:Result)    ! true         |
             xonly                        ! anError             ! true         |
             xonly                        ! skipped             ! false        |
             xonly                        ! success             ! false        |
             onlyStatus("x!")             ! failure             ! true         |
             onlyStatus("x!")             ! anError             ! true         |
             onlyStatus("x!")             ! skipped             ! false        |
             onlyStatus("x!")             ! success             ! false        |
             onlyStatus("o")              ! failure             ! false        |
             onlyStatus("o")              ! anError             ! false        |
             onlyStatus("o")              ! skipped             ! true         |
             onlyStatus("o")              ! success             ! false        |
             { (a, s, r) =>  a.showStatus(s.status) must_== r }


}
