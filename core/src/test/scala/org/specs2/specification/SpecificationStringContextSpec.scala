package org.specs2
package specification

import runner.MockClassRunner

class SpecificationStringContextSpec extends Specification { def is = s2"""

 A user specification can use string interpolation to write the specification fragments ${
  val lines = MockClassRunner().run(new UserInterpolatedSpecification)
  val expected = Seq(
    "user specification",
    "This is an introduction",
    "\\+ One example",
    "x Another example with a failure",
    "ko",
    "! A third example with an error",
    "A normal interpolated value: 100",
    "A normal interpolated string: hello",
    "A string hello followed by a Result",
    "o A skipped result",
    "An interpolated value with an error \\[undefined\\]",
    "Total for specification user specification"
  )
  forall(expected) { line => lines.map(_.replace("\n", "")) showAs (ls => ls.mkString("\n")) must containMatch(line) }
}

 The text before a result must be splitted into a Text fragment and the example description
   for a single line text                                                                 $e1
   for an auto-example (no text on the line)                                              $e2
   for a multiple line text
     when the last line is indented it is taken as the description                        $e3
     when the last line is not indented the full paragraph is taken as the description    $e4
     for an auto-example (no text on the last line)                                       $e5

"""

  def e1 = createDescription("example1", "ok")              === (("\n", FormattedString("example1").withFlow))
  def e2 = createDescription("  ", "ok")                    === (("  ", FormattedString.code("ok").withFlow))
  def e3 = createDescription("intro\n  ex1", "ok")          === (("intro\n  ", FormattedString("ex1").withFlow))
  def e4 = createDescription("long\nexample", "ok")         === (("\n", FormattedString("long\nexample").withFlow))
  def e5 = createDescription("long\nautoexample\n  ", "ok") === (("long\nautoexample\n  ", FormattedString.code("ok").withFlow))

}

class UserInterpolatedSpecification extends Specification { def is = s2"""  ${"user specification".title}
 This is an introduction.

 And some text
      with some code

 1. one ${i.toString}
 1. two
 1. three

 One example $e1
 Another example with a failure $e2

 A third example with an error $e3

 A normal interpolated value: ${i.toString}
 A normal interpolated string: $s
 A string $s followed by a Result $ok
 A skipped result $skipped
 An interpolated value with an error $errorValue

"""

  val i = 100
  val s = "hello"
  def errorValue = { sys.error("undefined"); "xxx" }

  def e1 = ok
  def e2 = ko
  def e3 = { sys.error("boom"); ok }

}

