package org.specs2
package main

import specification.After

class ArgumentsSpec extends SpecificationWithJUnit { def is =
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
                                                                                                                        p^
  "Some boolean arguments have negated names, like nocolor, meaning !color"                                             ! e7^
                                                                                                                        p^
  "An Arguments instance can be overriden by another with the `<|` operator: `a <| b`"                                  ^
    "if there's no corresponding value in b, the value in a stays"                                                      ! e8^
    "there is a corresponding value in b, the value in a is overriden when there is one"                                ! e9^
    "there is a corresponding value in b, the value in b is kept"                                                       ! e10^
                                                                                                                        p^
  "Arguments can also be passed from system properties"                                                                 ^
    "a boolean value just have to exist as -Dname"                                                                      ! e11^
    "a string value will be -Dname=value"                                                                               ! e12^
    "properties can also be passed as -Dspecs2.name to avoid conflicts with other properties"                           ! e12^
                                                                                                                        end


  def e1 = Arguments("xonly").xonly must beTrue
  def e2 = Arguments("specName", "spec").specName must_== "spec"

  def e3 = Arguments("").xonly must beFalse
  def e4 = Arguments("").specName must_== ".*Spec"

  def e5 = Arguments("xOnly").xonly must beTrue
  def e6 = Arguments("specname", "spec").specName must_== "spec"

  def e7 = Arguments("nocolor").color must beFalse

  def e8 = (args(xonly = true) <| args(plan = false)).xonly must_== true
  def e9 = args(xonly = true).overrideWith(args(xonly = false)).xonly must_== false
  def e10 = (args(xonly = true) <| args(plan = true)).plan must_== true

  object props extends After {
    def after = {
      System.getProperties.clear()
    }
  }
  def e11 = props {
    System.setProperty("plan", "")
    Arguments("").plan must_== true
  }
  def e12 = props {
    System.setProperty("specname", "spec")
    Arguments("").specName must_== "spec"
  }
  def e13 = props {
    System.setProperty("specs2.specname", "spec")
    Arguments("").specName must_== "spec"
  }
}