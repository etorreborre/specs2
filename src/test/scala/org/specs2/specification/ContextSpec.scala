package org.specs2
package specification
import io._

class ContextSpec extends Specification with FeaturesResults with ContextData with ExampleExecution {
  val examples = """
It is sometimes necessary to provide functions to "prepare" the specification before executing the examples and
clean it up afterwards. This may be for example:
   * opening a database connection
   * inserting some data
   * executing the example
   * closing the connection after each example
   
It may also be very convenient to have each example executed "inside" a specific context, like a 
web application session. Finally, some setups or cleanups are very expensive so one might want
to add arbitrary actions that will be executed only once

All of this can be achieved in specs2 by using case classes which extend the following traits:
   * Before
   * After
   * Around
   * BeforeAfter or BeforeAfterAround for combined functionality
"""^
"The Before trait can be used to"^
  "execute a method before the first example" ! c(e1)^
  "execute a method before the second example" ! c(e2)^
par^
"If the before method throws an exception"^
  "the first example will not execute" ! c(e3)^
  "it will be reported as an error" ! c(e4)^
par^
"The After trait can be used to"^
  "execute a method after the first example" ! c(e5)^
  "execute a method after the second example" ! c(e6)^
par^
"If the after method throws an exception"^
  "the first example will execute" ! c(e7)^
  "the first example will be reported as an error" ! c(e8)^
par^
"The Around trait can be used to"^
  "execute the example inside a user provided function" ! c(e9)^
par^
"The BeforeAfter trait can be used to"^
  "execute a method before and after the first example" ! c(e10)^
par^
"The BeforeAfterAround trait can be used to"^
  "execute a method before, around and after the first example" ! c(e11)
  
  def e1 = executing(ex1Before).prints("before", "e1")
  def e2 = executing(ex1_2Before).prints("before", "e1", "before", "e2")
  def e3 = executing(ex1_beforeFail).prints()
  def e4 = executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
  
  def e5 = executing(ex1After).prints("e1", "after")
  def e6 = executing(ex1_2After).prints("e1", "after", "e2", "after")
  def e7 = executing(ex1_afterFail).prints("e1")
  def e8 = executeBodies(ex1_beforeFail).map(_.message) must_== List("error")
  def e9 = executing(ex1Around).prints("around", "e1")
  def e10 = executing(ex1BeforeAfter).prints("before", "e1", "after")
  def e11 = executing(ex1BeforeAfterAround).prints("before", "around", "e1", "after")

  def executing(exs: Examples): Executed = Executed(executeBodies(exs))
  case class Executed(r: List[Result]) {
	def prints(messages: String*) = {
	  c.messages must_== List(messages:_*)
    }  
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
  object ar extends Around {
	def around[T <: Result](a: =>T): T = { c.println("around"); a } 
  }
  object ba extends BeforeAfter {
	def before = c.println("before")
	def after = c.println("after")
  }
  object baa extends BeforeAfterAround {
	def before = c.println("before")
	def after = c.println("after")
	def around[T <: Result](a: =>T): T = { c.println("around"); a } 
  }

  def ex(name: String) = { c.println(name); success }
  def ex1 = ex("e1")
  def ex2 = ex("e2")
  def ex1Before = "ex1" ! b(ex1)  
  def ex1_beforeFail = "ex1" ! b2(ex1) 
  def ex1_2Before = ex1Before ^ "ex2" ! b(ex2)

  def ex1After = "ex1" ! a(ex1) 
  def ex1_afterFail = "ex1" ! a2(ex1) 
  def ex1_2After = ex1After ^ "ex2" ! a(ex2)

  def ex1Around = "ex1" ! ar(ex1) 
  def ex1BeforeAfter = "ex1" ! ba(ex1) 
  def ex1BeforeAfterAround = "ex1" ! baa(ex1) 
}