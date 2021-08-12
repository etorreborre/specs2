package org.specs2
package matcher

import StringMatchers.given

trait ScalaInterpreterMatchers extends Expectations:
  private val outer = this

  /** use your own interpreter implementation to interpret a Scala script */
  def interpret(s: String): String

  given Conversion[String, InterpretedScript] with
    def apply(s: String): InterpretedScript =
      new InterpretedScript(s):
        def interpret = outer.interpret(s)

  abstract class InterpretedScript(s: String):
    protected def interpret: String

    /** execute the script and use the expected result to as a regular expression to check the validity of the actual
      * result
      */
    def >|(result: String) = BeMatching.withPart(result).apply(createExpectable(interpret))
