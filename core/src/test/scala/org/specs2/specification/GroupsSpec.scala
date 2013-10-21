package org.specs2
package specification

import execute.AsResult
import matcher.{ResultMatchers, ThrownExpectations}

class GroupsSpec extends script.Specification with Grouped with ThrownExpectations with ResultMatchers { def is = s2"""

 Example groups can be used in Specifications to declare the examples code that is to be inserted in the Specification text.

 There are 2 types of example groups, depending on the trait that is used:

 * in the `Groups` trait, every time an example is executed, the `ExampleGroup` owning the example will be re-created
    this provides the ability to get "fresh" variables for the example

 * in the `Grouped` trait, there is no such re-creation all variables, if any, are shared

 There is a limit of 22 groups of 22 examples each which are available in the `Groups` and `Grouped` traits but you can also use auto-numbering groups with no such limit.

Examples execution
==================

 The examples of a group must only be evaluated when the example is evaluated
   + for a numbered `Groups` group
   + for a numbered `Grouped` group
   + for an auto-numbered `Groups` group
   + for an auto-numbered `Grouped` group

Pending examples
================

 If an example is not defined, then its result is PENDING with the name of the group+example
   + for a numbered `Groups` group
   + for a numbered `Grouped` group
   + for an auto-numbered `Groups` group
   + for an auto-numbered `Grouped` group
   """

  "execution" - new group {
    def execute(g: TestedGroups) = {
      "the example is not evaluated" ==> { g.evaluated === false }
      "evaluate example is ok"       ==> { AsResult(g.group(0).example(0)) }
      "the example is evaluated"     ==> { g.evaluated === true  }
    }
    trait TestedGroups extends GroupsLike {
      var evaluated = false
    }
    eg := execute {
      new TestedGroups with Groups { "g1" - new g1 { e1 := { evaluated = true; ok } } }
    }
    eg := execute {
      new TestedGroups with Grouped { "g1" - new g1 { e1 := { evaluated = true; ok } } }
    }
    eg := execute {
      new TestedGroups with Groups { "g1" - new group { eg := { evaluated = true; ok } } }
    }
    eg := execute {
      new TestedGroups with Grouped { "g1" - new group { eg := { evaluated = true; ok } } }
    }
  }

  "pending example" - new group {
    def execute(g: TestedGroups) = {
      val result = (new runner.TextRunner).apply { new script.Specification { def is = nocolor ^ s2"""
        + example 1
        + example 2
        """
        override def group(i: Int) = g.group(i)
      }.content }.split("\n").toSeq

      "the first example is ok"       ==> { result must containMatch("\\+ example 1") }
      "the second example is pending" ==> { result must (containMatch[String]("PENDING") and containMatch("g1.e2")) }
    }
    trait TestedGroups extends GroupsLike
    eg := execute {
      new TestedGroups with Groups { "g1" - new g1 { e1 := ok } }
    }
    eg := execute {
      new TestedGroups with Grouped { "g1" - new g1 { e1 := ok } }
    }
    eg := execute {
      new TestedGroups with Groups { "g1" - new group { eg := ok } }
    }
    eg := execute {
      new TestedGroups with Grouped { "g1" - new group { eg := ok } }
    }
  }

}
