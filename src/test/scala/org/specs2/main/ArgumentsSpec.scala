package org.specs2
package main

class ArgumentsSpec extends SpecificationWithJUnit { def is = 
                                                                                          """
  Arguments can be passed on the command line as an Array of Strings.
  There are 2 types of arguments:
  
    * boolean arguments which only presence means that their value is true
      e.g. -xonly to show only failures and errors
      
    * string arguments which have a specific value
      e.g. -srcDir src/test to specify the directory holding the source files
      
                                                                                          """^
                                                                                          p^
  { Arguments(Seq[String]():_*).xonly must beFalse }                                       ^
  { Arguments("xonly").xonly must beTrue }                                                ^
  { Arguments("xonly", "srcDir", "src/test").srcDir must_== "src/test" }                  ^
                                                                                          p^
  "The argument names can be capitalized or not"                                          ^
  { Arguments("srcdir", "src/test").srcDir must_== "src/test" }                           ^
  { Arguments("xOnly").xonly must beTrue}                                                 ^
                                                                                          end^
  "If an argument is not provided, its default value is used"                             !
  { Arguments("xonly").srcDir must_== "src/test/scala/" }                                 ^
                                                                                          p^
  "It is also possible to pass other default values, via an Arguments objec"              ^
  { Arguments(Arguments(srcDir = "src"))("xonly").srcDir must_== "src" }                                 ^
                                                                                          end
}