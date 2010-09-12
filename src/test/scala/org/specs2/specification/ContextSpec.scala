package org.specs2
package specification
import io._

class ContextSpec extends Specification with FeaturesResults with ContextData with ExampleExecution {
  val examples = 
"""It is sometimes necessary to provide functions to "prepare" the specification before executing the examples and
   clean it up afterwards. This may be for example:
   * opening a database connection
   * inserting some data
   * executing the example
   * closing the connection after each example
   
It may also be very convenient to have each example executed "inside" a specific context, like a 
web application session.

All of this can be achieved in specs2 by using case classes which extend the following traits:
   * Before
   * After
   * Around"""^^
"The Before trait can be used to"^
  "execute a method before the first example" ! c(e1)^
  "execute a method before the second example" ! c(e2)^
"If the before method throws an exception"^
  "the first example will not execute" ! c(e3)^
  "it will be reported as an error" ! c(e4)
  
  def e1 = executing(ex1)("before", "e1")(b)
  def e2 = executing(ex1_2)("before", "e1", "before", "e2")(b)
  def e3 = executing(ex1_beforeFail)()(b2)
  def e4 = executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
  def executing(exs: Examples)(messages: String*)(c: MockOutput) = {
	executeBodies(exs)
	c.messages must_== List(messages:_*)
  }
}
trait ContextData extends FeaturesResults with ExamplesBuilder {
  object c extends Before {
	def before = b.clear()
  }
  object b2 extends Before with MockOutput {
	def before = Predef.error("error")
  }
  object b extends Before with MockOutput {
	def before = println("before")
  }
  def ex1 = "ex1" ! b { b.println("e1"); success } 
  def ex1_beforeFail = "ex1" ! b2 { b2.println("e1"); success } 
  def ex1_2: Examples = (ex1 ^ "ex2" ! b { b.println("e2"); success })
}