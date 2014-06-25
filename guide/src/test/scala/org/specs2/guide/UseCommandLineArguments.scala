package org.specs2
package guide

import main._
import specification.CommandLineArguments
import specification.core.foreachInSequence

object UseCommandLineArguments extends UserGuidePage { def is = s2"""

Some specifications need to be fine-tuned and slightly modified. Sometimes to access a specific environment, or to disable some examples, or to execute more ScalaCheck properties. For all those situations it is desirable to modify the specification directly from the command-line without having to recompile it.

## Control an example

Let's see first how to use the command line to modify the outcome of just one example:${snippet{
class SpecificationWithArgs extends Specification { def is = s2"""
 This example is controlled from the command line $e1
"""

  def e1 = (commandLine: CommandLine) =>
    if (commandLine.isDefined("isOk")) 1 must_== 1
    else                               1 must_== 2
}
}}

With a mutable specification the code is similar:${snippet{
class SpecificationWithArgs extends mutable.Specification {
 "This example is controlled from the command line" in { commandLine: CommandLine =>
   if (commandLine.isDefined("isOk")) 1 must_== 1
   else                               1 must_== 2
 }
}
}}

## Control a specification

You can also drive the creation of the full specification with command line arguments:${snippet{
class SpecificationWithArgs extends Specification with CommandLineArguments { def is(commandLine: CommandLine) =
  if (commandLine.isDefined("small"))
s2"""
 This is a small specification
  with one example $e1
"""
  else
s2"""
 This is a BIG specification
  with many examples ${ (1 to 1000).repeat(i => "ex"+i ! ok) }
"""
  def e1 = ok
}
}}

For a mutable specification we can use almost the same syntax but the `CommandLineArguments` trait must come from the `org.specs2.specification.mutable` package:${snippet{
class SpecificationWithArgs extends mutable.Specification with specification.mutable.CommandLineArguments { def is(commandLine: CommandLine) =
  if (commandLine.isDefined("small"))
    "This is a small specification" should {
      "with one example" in { 1 must_== 1 }
    }
  else
    "This is a small specification" >> {
      "with lots of examples" >> (1 to 1000).repeat(i => "ex"+i >> ok)
    }
}
}}

"""
}
