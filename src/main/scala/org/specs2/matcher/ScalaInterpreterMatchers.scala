package org.specs2
package matcher


trait ScalaInterpreterMatchers extends Expectations { outer =>
  /** use your own interpreter implementation to interpret a Scala script */
  def interpret(s: String): String
  implicit def interpretedScript(s: String): InterpretedScript = new InterpretedScript(s) {
    def interpret = outer.interpret(s)
  }

  abstract class InterpretedScript(s: String) {
    protected def interpret: String
    /** execute the script and use the expected result to as a regular expression to check the validity of the actual result */
    def >|(result: String) = new BeMatching(".*"+result+".*").apply(createExpectable(interpret))
  }
}


