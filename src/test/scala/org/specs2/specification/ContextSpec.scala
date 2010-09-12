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
  "execute a method before the second example" ! c(e2)
  
  def e1 = {
	executeBodies(ex1)
	b.messages must_== List("before", "e1")
  }
  def e2 = {
	executeBodies(ex1_2)
	b.messages must_== List("before", "e1", "before", "e2")
  }
}
trait ContextData extends FeaturesResults with ExamplesBuilder {
  object c extends Before {
	def before = b.clear()
  }
  object b extends Before with MockOutput {
	def before = println("before")
  }
  def ex1 = "ex1" ! b { b.println("e1"); success } 
  def ex1_2: Examples = (ex1 ^ "ex2" ! b { b.println("e2"); success })
}