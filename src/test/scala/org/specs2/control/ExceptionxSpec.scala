package org.specs2
package control
import execute._
import specification._

class ExceptionxSpec extends Specification with Exceptionx {
  val examples =
"""  
The Exceptionx trait provide extensions to regular exceptions
"""^
  "It has location methods"^
    "to get the name of the file and the line from an exception" ! c().e1^
    "to get the class name and line number of an exception" ! c().e2
  
  case class c() extends ExceptionxContext {
	def e1 = e.location must_== "ExceptionxContext.scala:4"
    def e2 = e.classLocation must_== "org.specs2.control.ExceptionxContext:4"
  }
}