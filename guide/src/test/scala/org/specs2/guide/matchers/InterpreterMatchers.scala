package org.specs2
package guide
package matchers

import form.Card
import matcher.ScalaInterpreterMatchers

object InterpreterMatchers extends UserGuideCard with matcher.ScalaInterpreterMatchers {
  def title = "Scala Interpreter"
  def text =  s2"""
In the rare case where you want to use the Scala interpreter and execute a script: ${snippet{

class ScalaInterpreterMatchersSpec extends mutable.Specification with ScalaInterpreterMatchers {
  def interpret(s: String): String = "" // you have to provide your own Scala interpreter here

  "A script" can {
    "be interpreted" in {
      "1 + 1" >| "2"
    }
  }
}
}}
"""
  def interpret(s: String) = s
}
