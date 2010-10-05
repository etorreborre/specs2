package org.specs2
package control
import execute._
import specification._

class ThrowablexSpec extends SpecificationWithJUnit with Throwablex {
  val content =
"""  
  The Throwablex trait provides extensions to regular throwables:
"""^
"  It provides methods to access nested exceptions"^
"    chainedExceptions returns a list of nested exceptions" ! chained().e1^
"    getFullStackTrace returns a list of all nested stackTraceElements" ! chained().e2^
end

  case class chained() extends ExceptionxContext {
	def e1 = e.chainedExceptions must_== List(e.getCause)
	def e2 = e.chainedExceptions must_== List(e.getCause)
  }
}