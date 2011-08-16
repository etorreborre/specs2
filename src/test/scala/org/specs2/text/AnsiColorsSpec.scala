package org.specs2
package text

class AnsiColorsSpec extends Specification { def is =

  "it is possible to remove the colors from a string"                                                                     ! e1^
  "coloring a string must keep newlines"                                                                                  ^
	  "if start and end with newline"                                                                                       ! e2^
	  "if start with newline"                                                                                               ! e3^
	  "if end with newline"                                                                                                 ! e4^
	  "if no newline"                                                                                                       ! e5^
	  "if empty"                                                                                                            ! e6^
                                                                                                                          end

  
	import AnsiColors._
	
	def e1 = removeColors("hello" + AnsiColors.red) must_== "hello"  
	
	def e2 = color("\nhello\n", "*") must_== "\n*hello"+reset+"\n"
	def e3 = color("\nhello", "*") must_== "\n*hello"+reset
	def e4 = color("hello\n", "*") must_== "*hello"+reset+"\n"
	def e5 = color("hello", "*") must_== "*hello"+reset
	def e6 = color("", "*") must_== ""
	
}