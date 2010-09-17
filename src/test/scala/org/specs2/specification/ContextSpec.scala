package org.specs2
package specification
import io._
import execute._

class ContextSpec extends SpecificationWithJUnit with StandardResults with ContextData with ExampleExecution {
  val examples = """
  It is sometimes necessary to provide functions to "prepare" the specification before executing the Fragments and
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
"  The Before trait can be used to execute methods before Fragments"^
"    the before method is executed before a first example" ! c(e1)^
"    the before method is executed before the second example" ! c(e2)^
p^
"  If the before method throws an exception"^
"    the first example will not execute" ! c(e3)^
"    and it will be reported as an error" ! c(e4)^
p^
"  The After trait can be used to execute methods after Fragments"^
"    the after method is executed after a first example" ! c(e5)^
"    the after method is executed after the second example" ! c(e6)^
p^
"  If the after method throws an exception"^
"    the first example will execute" ! c(e7)^
"    but it will be reported as an error" ! c(e8)^
p^
"  The Around trait can be used to"^
"    execute the example inside a user provided function" ! c(e9)^
p^
"  The BeforeAfter trait can be used to"^
"    execute a method before and after each example" ! c(e10)^
p^
"  The BeforeAfterAround trait can be used to"^
"    execute a method before, around and after the first example" ! c(e11)^
p^
"  An Action can be used to create Step fragments containing an action to execute:"^
"    val first = new Action"^
"    val examples = first(c.println('first')) ^ ex1"^
p^
"    that action will execute and return a result" ! c(e12)^
"    if it executes ok, nothing is printed, it is a silent Success" ! c(e13)^
"    otherwise, it is reported as an Error" ! c(e14)
  
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
  def e12 = executing(firstThenEx1).prints("first", "e1")
  def e13 = executeBodies(silentFirstThenEx1).map(_.message) must_== List("success")
  def e14 = executeBodies(failingFirstThenEx1).map(_.message) must_== List("error", "success")

  def executing(exs: Fragments): Executed = Executed(executeBodies(exs))
  case class Executed(r: List[Result]) {
	def prints(messages: String*): Result = {
	  c.messages must_== List(messages:_*)
    }  
  }
}
trait ContextData extends StandardResults with FragmentsBuilder with ContextsForFragments {

  def ok(name: String) = { c.println(name); success }
  def ok1 = ok("e1")
  def ok2 = ok("e2")
  
  def ex1 = "ex1" ! ok1  
  def ex1Before = "ex1" ! before(ok1)  
  def ex1_beforeFail = "ex1" ! beforeWithError(ok1) 
  def ex1_2Before = ex1Before ^ "ex2" ! before(ok2)

  def ex1After = "ex1" ! after(ok1) 
  def ex1_afterFail = "ex1" ! afterWithError(ok1) 
  def ex1_2After = ex1After ^ "ex2" ! after(ok2)

  def ex1Around = "ex1" ! around(ok1) 
  def ex1BeforeAfter = "ex1" ! beforeAfter(ok1) 
  def ex1BeforeAfterAround = "ex1" ! beforeAfterAround(ok1)
  
  val first = new Action
  def firstThenEx1 = first(c.println("first")) ^ ex1
  def silentFirstThenEx1 = first("first") ^ ex1
  def failingFirstThenEx1 = first(Predef.error("error")) ^ ex1
}
trait ContextsForFragments {
  object c extends Before with MockOutput {
	def before = clear()
  }

  object before extends Before {
	def before = c.println("before")
  }
  object beforeWithError extends Before with MockOutput {
	def before = Predef.error("error")
  }
  object after extends After {
	def after = c.println("after")
  }
  object afterWithError extends After {
	def after = Predef.error("error")
  }
  object around extends Around {
	def around[T <% Result](a: =>T) = { c.println("around"); a } 
  }
  object beforeAfter extends BeforeAfter {
	def before = c.println("before")
	def after = c.println("after")
  }
  object beforeAfterAround extends BeforeAfterAround {
	def before = c.println("before")
	def after = c.println("after")
	def around[T <% Result](a: =>T) = { c.println("around"); a } 
  }
}