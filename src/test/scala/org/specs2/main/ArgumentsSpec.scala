package org.specs2
package main

import matcher.DataTables
import execute.Result
import specification._

class ArgumentsSpec extends script.Specification with DataTables with Grouped { def is = s2"""

Arguments can be passed on the command line as an Array of Strings. There are 2 types of arguments:

 * boolean arguments which only presence means that their value is true
   e.g. `xonly` to show only failures and errors

 * string arguments which have a specific value
   e.g. `srcTestDir src/test` to specify the directory holding the source files
                                                                                                                        
Definition
==========

  If an argument is specified, its value is returned                                                                  
    + for a boolean argument like xonly the value is true
    + for a string argument, it is the 'next' value

  If an argument is not specified, its default value is returned
    + for a boolean argument like xonly, it is false
    + for a string argument like specName, it is .*Spec

  The argument names can be capitalized or not
    + for a boolean argument like xonly, xOnly is admissible
    + for a string argument like specName, specname is admissible
    + but the name has to match exactly, 'exclude' must not be mistaken for 'ex'

  + Some boolean arguments have negated names, like nocolor, meaning !color

Overriding
==========

  An Arguments instance can be overriden by another with the `<|` operator: `a <| b`
    + if there's no corresponding value in b, the value in a stays
    + there is a corresponding value in b, the value in a is overriden when there is one
    + there is a corresponding value in b, the value in b is kept

System props
============

  Arguments can also be passed from system properties
    + a boolean value just have to exist as -Dname
    + a boolean value can be -Dname=true
    + a boolean value can be -Dname=false
    + a string value will be -Dname=value
    + properties can also be passed as -Dspecs2.name to avoid conflicts with other properties

Execution
=========

  Arguments can decide if a result must be shown or not, depending on its status
    + xonly => canShow(x)
    + xonly => canShow(result.status)

  Some values can be filtered from the command line
    + to include only some arguments
    + to exclude some arguments
                                                                                                               """


  "values" - new group {
    eg := Arguments("xonly").xonly must beTrue
    eg := Arguments("specName", "spec").specName must_== "spec"

    eg := Arguments("").xonly must beFalse
    eg := Arguments("").specName must_== ".*Spec"

    eg := Arguments("xOnly").xonly must beTrue
    eg := Arguments("specname", "spec").specName must_== "spec"
    eg := Arguments("exclude", "spec").ex must_== Arguments().ex

    eg := Arguments("nocolor").color must beFalse
   }

  "overriding" - new group {
    eg := (args(xonly = true) <| args(plan = false)).xonly must_== true
    eg := args(xonly = true).overrideWith(args(xonly = false)).xonly must_== false
    eg := (args(xonly = true) <| args(plan = true)).plan must_== true
  }

  "properties" - new group {
     case class properties(map:(String, String)*) extends MapSystemProperties {
       lazy val properties = Map(map:_*)
     }

    eg := Arguments.extract(Seq(""), properties("plan" -> "")).plan must_== true
    eg := Arguments.extract(Seq(""), properties("plan" -> "true")).plan must_== true
    eg := Arguments.extract(Seq(""), properties("plan" -> "false")).plan must_== false
    eg := Arguments.extract(Seq(""), properties("specname" -> "spec")).specName must_== "spec"
    eg := Arguments.extract(Seq(""), properties("specs2.specname" -> "spec")).specName must_== "spec"
   }

  "execution" - new group {
    eg := "args"                      | "status" | "canShow"    |>
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
          { (a, s, r) =>  a.canShow(s) must_== r }

     eg := "args"                      | "status"            | "canShow"    |>
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
            { (a, s, r) =>  a.canShow(s.status) must_== r }

    eg := Arguments("this", "is", "cool").commandLineFilter("this", "cool").commandLine.arguments === Seq("this", "cool")
    eg := Arguments("this", "is", "cool").commandLineFilterNot("this", "cool").commandLine.arguments === Seq("is")
  }
}
