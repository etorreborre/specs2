package org.specs2
package control

import mutable.Specification
import io.MockOutput
import specification.Scope
import scala.collection.mutable.ListBuffer

class DebugSpec extends Specification {
  "It is possible to insert some method call to print values" >> {
    "pp prints a value and returns it" >> new output {
      Value(1).pp === Value(1)
      messages === Seq("Value(1)")
    }
    "pp(condition) prints a value but only if a condition is true" >> new output {
      Value(1).pp(condition = true)
      messages === Seq("Value(1)")

      clearMessages

      Value(1).pp(condition = false)
      messages === Seq()
    }
    "pp(condition function) prints a value but only if a condition on the value is true" >> new output {
      Value(1).pp((v: Value) => v.i == 1)
      messages === Seq("Value(1)")

      clearMessages

      Value(1).pp((v: Value) => v.i == 2)
      messages === Seq()
    }
    "pp(show) prints a value with a specific show function" >> new output {
      Value(1).pp((v: Value) => "v = "+v.i)
      messages === Seq("v = 1")
    }
    "pp(pre) prints a value with a message before" >> new output {
      Value(1).pp("the value is")
      messages === Seq("the value is Value(1)")
    }
  }

  trait output extends Scope {
    var msgs = ListBuffer[String]()
    def messages = msgs.toSeq
    def clearMessages = msgs.clear
    implicit def debug[T](t: =>T): Debuggable[T] = new DebuggableMock(t)
    class DebuggableMock[T](t: =>T) extends Debuggable(t) with MockOutput {
      override def append(msg: String) = msgs += msg
    }
  }
  case class Value(i: Int)
}
