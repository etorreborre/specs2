package org.specs2
package control
import execute._
import specification._

class ExceptionxSpec extends Specification with Exceptionx {
  val content =
"""  
  The Exceptionx trait provides extensions to regular exceptions:
"""																		                                 ^
"  It has location methods"												                     ^
"    to get the name of the file and the line from an exception" 		   ! c().e1^
"    to get the class name and line number of an exception" 			     ! c().e2^
"    to get the class name, file name and line number of an exception" ! c().e3^
																		                                   p^
"  It allows to filter stacktraces"										                 ^
"    to filter all the lines matching a given pattern" 					       ! c().e4^
"    to filter all the lines not matching a given pattern" 				     ! c().e5^
																		                                   p^
"  It provides utility functions for stacktrace elements"				       ^
"    apply returns the ith element" 									                 ! c().e6^
"    headOption returns the first element as an option" 				       ! c().e7^
																		                                   end

  case class c() extends ExceptionxContext {
	def e1 = e.location must_== "ExceptionxContext.scala:6"
    def e2 = e.classLocation must_== "org.specs2.control.ExceptionxContext:6"
    def e3 = e.fullLocation must_== "org.specs2.control.ExceptionxContext (ExceptionxContext.scala:6)"
    def e4 = e.filter("org.specs2.control").getStackTrace.toList.map(_.toString) must 
               containMatch("org.specs2.control")
    def e5 = e.filterNot("org.specs2.control").getStackTrace.toList.map(_.toString) must 
               containMatch("org.specs2.control").not
    def e6 = e(2).toString must beMatching(".*apply.*")
    def e7 = e.headOption.map(_.toString).toIterable must containMatch("ExceptionxContext")
  }
}