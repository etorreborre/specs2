package org.specs2
package guide

import main._
import execute.AsResult
import org.specs2.specification.core.Fragment
import org.specs2.specification.{BeforeAfterAll, CommandLineArguments, ContextWithCommandLineArguments, ForEachWithCommandLineArguments, Before}

object UseCommandLineArguments extends UserGuidePage { def is = "Use command-line arguments".title ^ s2"""

Some specifications need to be fine-tuned and constantly modified. Sometimes to access a specific environment, or to disable some examples, or to execute more ScalaCheck properties. For all those situations it is desirable to modify the specification directly from the command-line without having to recompile it.

### Control an example

Let's see first how to use the command line to modify the outcome of just one example:${snippet{
class SpecificationWithArgs extends Specification { def is = s2"""
 This example is controlled from the command line $e1
"""

  def e1 = (commandLine: CommandLine) =>
    if (commandLine.isSet("isOk")) 1 must_== 1
    else                           1 must_== 2
}
}}

With a mutable specification the code is similar:${snippet{
class SpecificationWithArgs extends mutable.Specification {
 "This example is controlled from the command line" in { commandLine: CommandLine =>
   if (commandLine.isSet("isOk")) 1 must_== 1
   else                           1 must_== 2
 }
}
}}

### Control a specification

There are 2 ways to drive the creation of the full specification with command line arguments.

#### CommandLineArguments trait

Mixing-in the `CommandLineArguments` trait allow you to pass the command line arguments in the `is` method: ${snippet{
class SpecificationWithArgs extends Specification with CommandLineArguments { def is(commandLine: CommandLine) =
  if (commandLine.isSet("small"))
s2"""
 This is a small specification
  with one example $e1
"""
  else
s2"""
 This is a BIG specification
  with many examples ${ Fragment.foreach(1 to 1000)(i => "ex"+i ! ok) }
"""
  def e1 = ok
}
}}

For a mutable specification we can use almost the same syntax but the `CommandLineArguments` trait must come from the `org.specs2.specification.mutable` package:${snippet{
class SpecificationWithArgs extends mutable.Specification with specification.mutable.CommandLineArguments {
  def is(commandLine: CommandLine) =
    if (commandLine.isSet("small"))
      "This is a small specification" should {
        "with one example" in { 1 must_== 1 }
      }
    else
      "This is a small specification" >> {
        "with lots of examples" >> Fragment.foreach(1 to 1000)(i => "ex"+i >> ok)
      }
}
}}

#### Dependency injection

Any specification with a 1-parameter constructor can be instantiated provided that:

 - the parameter has itself a constructor with no parameters or a 1-parameter constructor which we can instantiate
 - the paramater is of type `Env`, `Arguments`, `CommandLine`

In particular this means that you can define a `Specification` with a constructor using a `CommandLine` argument and when the specification will be created it will be passed the command line arguments: ${snippet{
case class MyDbSpec(commandLine: CommandLine) extends Specification with DbSpec { def is = s2"""

  create a user $createUser

"""
  // the database client is created from the command line
  // arguments and can be used in the examples
  def createUser = client.createUser("xxx") must beSome
}

// Template trait for accessing the database
// this trait can be controlled from command line arguments
// and it takes care of the setup of the database before and after all
// the examples
trait DbSpec extends Specification with BeforeAfterAll {
  def commandLine: CommandLine

  def beforeAll = println("start db here")
  def afterAll  = println("stop db here")

  lazy val client = {
    if (commandLine.contains("prod")) DbClient("production")
    else                              DbClient("test")
  }

  case class DbClient(env: String) {
    def createUser(name: String): Option[String] = ???
  }
}
}}


### Control a context

The next thing you might want to control is contexts. Instead of using the `BeforeEach` / `AfterEach` / `AroundEach` traits directly you will need to implement the `ContextWithCommandLineArguments` trait and provide the appropriate context object:${snippet{
class SpecificationWithArgs extends Specification with ContextWithCommandLineArguments { def is = s2"""
 This is a specification with a context depending on command line arguments
  with one example $ok
"""
  /** you need to define this method */
  def context = (commandLine: CommandLine) =>
    new Before {
      def before = if (commandLine.isSet("dobefore")) println("before!")
    }
}
}}

### Control data injection

The final situation where you would need to use command-line arguments is with a `ForEach` trait. If you want to influence the injection of data with the command line, the `ForEachWithCommandLineArguments` trait needs to be mixed in:${snippet{
class SpecificationWithArgs extends Specification with ForEachWithCommandLineArguments[Int] { def is = s2"""
 This is a specification
  with one example using injected data ${ (i: Int) => i must_== i }
"""
  /** you need to define this method */
  def foreach[R : AsResult](commandLine: CommandLine)(f: Int => R) =
    AsResult(f(commandLine.int("value").getOrElse(0)))
}
}}

And for a mutable specification:${snippet{
class SpecificationWithArgs extends mutable.Specification with specification.mutable.ForEachWithCommandLine[Int] {
  "This is a specification" >> {
    "with one example using injected data" >> { (i: Int) =>
      i must_== i
    }
   }

   /** you need to define this method */
   def foreach[R : AsResult](commandLine: CommandLine)(f: Int => R) =
     AsResult(f(commandLine.int("value").getOrElse(0)))
}
}}
"""
}
