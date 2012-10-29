package org.specs2
package control

import mutable.Specification
import io.MockOutput
import specification.Scope
import scala.collection.mutable.ListBuffer
import java.awt.print.Printable
import matcher.MustMatchers
import control.Debug.Debuggable

class DebugSpec extends Specification {

  "It is possible to insert some method call to print values" >> {
    "pp prints a value and returns it" >> new output {
      (Value(1).pp === Value(1)) prints "Value(1)"
    }
    "pp(condition) prints a value but only if a condition is true" >> new output {
      Value(1).pp(condition = false) prints nothing
      Value(1).pp(condition = true) prints "Value(1)"
    }
    "pp(condition function) prints a value but only if a condition on the value is true" >> new output {
      Value(1).pp((v: Value) => v.i == 2) prints nothing
      Value(1).pp((v: Value) => v.i == 1) prints "Value(1)"
    }
    "pp(show) prints a value with a specific show function" >> new output {
      Value(1).pp((v: Value) => "v = "+v.i) prints "v = 1"
    }
    "pp(pre) prints a value with a message before" >> new output {
      Value(1).pp("the value is") prints "the value is Value(1)"
    }
  }

  case class Value(i: Int)
}

trait output extends Scope with MustMatchers {
  private var msgs = ListBuffer[String]()
  private def messages = msgs.toSeq

  // this implicit intercepts appended messages from the calls to 'pp'
  implicit def debug[T](t: =>T): Debuggable[T] = new DebuggableMock(t)
  class DebuggableMock[T](t: =>T) extends Debuggable(t) with MockOutput {
    override def append(msg: String) = msgs += msg
  }

  implicit def printable[T](t: =>T): Printable[T] = new Printable(t)
  class Printable[T](t: =>T) {
    def prints(results: String*) = {
      t
      if (results == Seq("nothing")) messages must beEmpty
      else                           messages must be_==(results)
    }
  }
  val nothing = "nothing"
}
