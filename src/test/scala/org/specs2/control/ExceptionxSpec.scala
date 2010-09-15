package org.specs2
package control
import execute._
import specification._

class ExceptionxSpec extends Specification with Exceptionx {
	override val args = "stacktrace"
  val examples =
"""  
The Exceptionx trait provide extensions to regular exceptions
"""^
  "It has location methods"^
    "to get the name of the file and the line from an exception" ! c().e1^
    "to get the class name and line number of an exception" ! c().e2^
    "to get the class name, file name and line number of an exception" ! c().e3^
  "It allows to filter stacktraces"^
    "to filter all the lines matching a given pattern" ! c().e4^
    "to filter all the lines not matching a given pattern" ! c().e5^
end

  case class c() extends ExceptionxContext {
	def e1 = e.location must_== "ExceptionxContext.scala:4"
    def e2 = e.classLocation must_== "org.specs2.control.ExceptionxContext:4"
    def e3 = e.fullLocation must_== "org.specs2.control.ExceptionxContext (ExceptionxContext.scala:4)"
    def e4 = e.filter("org.specs2.control").getStackTrace.size must_== 5
    def e5 = e.filterNot("org.specs2.control").getStackTrace.size must_== 23
  }
}