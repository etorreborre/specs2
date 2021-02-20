package org.specs2
package control

import org.specs2.io.ConsoleOutput
import org.specs2.matcher.MustThrownMatchers

import scala.collection.mutable.ListBuffer

class DebugSpec extends Specification { def is = s2"""

  It is possible to insert some method calls to print values on the console
    pp prints a value and returns it ${
      val output = new output {}; import output.{given, _}
      (Value(1).pp) prints "Value(1)"
    }
    pp(condition) prints a value but only if a condition is true" ${
      val output = new output {}; import output.{given, _}
      Value(1).pp(condition = false) prints nothing
      Value(1).pp(condition = true) prints "Value(1)"
    }
    pp(condition function) prints a value but only if a condition on that value is true" ${
      val output = new output {}; import output.{given, _}
      Value(1).pp((v: Value) => v.i == 2) prints nothing
      Value(1).pp((v: Value) => v.i == 1) prints "Value(1)"
    }
    pp(show) prints a value with a specific show function ${
      val output = new output {}; import output.{given, _}
      Value(1).pp((v: Value) => "v = "+v.i) prints "v = 1"
    }
    pp(pre) prints a value with a prepended message ${
      val output = new output {}; import output.{given, _}
      Value(1).pp("the value is") prints "the value is Value(1)"
    }}

  """
  case class Value(i: Int)
}

trait output extends MustThrownMatchers:
  private val msgs = ListBuffer[String]()
  def messages = msgs.toList

  given stringOutput: ConsoleOutput = new ConsoleOutput:
    override def println(m: Any) =
      msgs += m.toString; ()

  extension [T](t: =>T)
    def prints(results: String*) =
      t
      if results == Seq("nothing") then
        messages must beEmpty
      else
        messages must be_==(results)

  val nothing = "nothing"
