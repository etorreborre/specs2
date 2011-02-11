package org.specs2
package matcher
import tools._

trait ScalaInterpreterMatchers extends ScalaInterpreter {
  implicit def interpretedScript(s: String): InterpretedScript = new InterpretedScript(s)
}

class InterpretedScript(s: String) {
  /** execute the script and use the expected result to as a regular expression to check the validity of the actual result */
  def >|(result: String) = new BeMatching(".*"+result+".*").apply(Expectable(Script(s).interpret))
}
