package org.specs2
package guide

import main.*
import execute.AsResult
import org.specs2.specification.core.Fragment
import org.specs2.specification.{BeforeAfterSpec, Before}

object UseCommandLineArguments extends UserGuidePage {
  def is = "Use command-line arguments".title ^ s2"""

Some specifications need to be fine-tuned and constantly modified. Sometimes to access a specific environment, or to disable some examples,
or to execute more ScalaCheck properties. For all those situations it is desirable to modify the specification directly from the command-line
without having to recompile it.

### Control an example

Let's see first how to use the command line to modify the outcome of just one example:${snippet {

    import org.specs2.main.*

    class SpecificationWithArgs(args: CommandLine) extends Specification:
      def is = s2"""

 This example is controlled from the command line $e1
 """

      def e1 =
        if (args.isSet("isOk"))
          1 must ===(1)
        else
          1 must ===(2)
  }}

With a mutable specification the code is similar:${snippet {
    class SpecificationWithArgs(args: CommandLine) extends mutable.Spec:
      "This example is controlled from the command line" >> {
        if (args.isSet("isOk"))
          1 must ===(1)
        else
          1 must ===(2)
      }
  }}

Then you can set the argument, or not in sbt with
```
sbt> testOnly *ArgsSpec*
sbt> testOnly *ArgsSpec* -- !isOk
sbt> testOnly *ArgsSpec* -- isOk
```

The first 2 invocations of the specification will report a failure (1st one, the argument is not set and for the second one it is negated)
 and the 3rd invocation will make the specification pass.

### Control a specification

Any specification with a 1-parameter constructor can be instantiated provided that:

 - the parameter has itself a constructor with no parameters or a 1-parameter constructor which we can instantiate
 - the parameter is of type `Env`, `ExecutionEnv`, `Arguments`, `CommandLine`
$p

In particular this means that you can define a `Specification` with a constructor using a `CommandLine` argument and when
the specification will be created it will be passed the command line arguments: ${snippet {
// 8<---
    case class DbClient(env: String):
      def createUser(name: String): Option[String] = ???
    trait DbSpec extends Specification:
      lazy val client: DbClient = ???
// 8<---
    case class MyDbSpec(commandLine: CommandLine) extends Specification with DbSpec:
      def is = s2"""

  create a user $createUser

  """
      // the database client is created from the command line
      // arguments and can be used in the examples
      def createUser = client.createUser("xxx") must beSome
  }}
```

// Template trait for accessing the database
// this trait can be controlled from command line arguments
// and it takes care of the setup of the database before and after all
// the examples
trait DbSpec extends Specification with BeforeAfterSpec:
  def commandLine: CommandLine

  def beforeSpec = step(println("start db here"))
  def afterSpec  = step(println("stop db here"))

  lazy val client = {
    if (commandLine.contains("prod")) DbClient("production")
    else                              DbClient("test")
  }

case class DbClient(env: String):
  def createUser(name: String): Option[String] = ???
```
"""
}
