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
  "it will be reported as an error" ! c(e4)^
br^
"The After trait can be used to"^
  "execute a method after the first example" ! c(e5)^
  "execute a method after the second example" ! c(e6)^
"If the after method throws an exception"^
  "the first example will execute" ! c(e7)^
  "the first example will be reported as an error" ! c(e8)
  
  def e1 = executing(ex1Before)("before", "e1")
  def e2 = executing(ex1_2Before)("before", "e1", "before", "e2")
  def e3 = executing(ex1_beforeFail)()
  def e4 = executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
  
  def e5 = executing(ex1After)("e1", "after")
  def e6 = executing(ex1_2After)("e1", "after", "e2", "after")
  def e7 = executing(ex1_afterFail)("e1")
  def e8 = executeBodies(ex1_beforeFail).map(_.message) must_== List("error")

  def executing(exs: Examples)(messages: String*) = {
	executeBodies(exs)
	c.messages must_== List(messages:_*)
  }
}
trait ContextData extends FeaturesResults with ExamplesBuilder {
  object c extends Before with MockOutput {
	def before = clear()
  }
  object b extends Before {
	def before = c.println("before")
  }
  object b2 extends Before with MockOutput {
	def before = Predef.error("error")
  }
  object a extends After {
	def after = c.println("after")
  }
  object a2 extends After {
	def after = Predef.error("error")
  }
  def ex1Before = "ex1" ! b { c.println("e1"); success } 
  def ex1_beforeFail = "ex1" ! b2 { c.println("e1"); success } 
  def ex1_2Before = (ex1Before ^ "ex2" ! b { c.println("e2"); success })

  def ex1After = "ex1" ! a { c.println("e1"); success } 
  def ex1_afterFail = "ex1" ! a2 { c.println("e1"); success } 
  def ex1_2After = (ex1After ^ "ex2" ! a { c.println("e2"); success })
}