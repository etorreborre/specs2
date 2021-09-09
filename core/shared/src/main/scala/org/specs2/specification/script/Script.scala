package org.specs2
package specification
package script

/** A Script is an object responsible for analysing a piece of text and creating a sequence of fragments.
  *
  * It is first created with a list of fragments executions (some code) and when the `fragments` method is called it can
  * associate each execution to some portion of text according to a template.
  *
  * For example a GWT script (a `Scenario`) stores functions to create Given/When/Then steps and examples and the
  * `LastLinesScriptTemplate` extracts the last lines of a piece of text, divides them into blocks of Given/When/Then
  * lines based on the number of steps in the Scenario.
  *
  * See the GWTSpec in the examples module
  */
trait Script:
  /** @return the title of the script */
  def title: String

  /** parse the passed text and return a list of corresponding fragments */
  def fragments(text: String): FragmentsSeq

  /** @return true if this object marks the beginning of the script */
  def isStart: Boolean

/** set of lines returned by a ScriptTemplate
  */
trait ScriptLines

/** A ScriptTemplate parses some text to create ScriptLines that the associated script knows how to translate to
  * Fragments.
  */
trait ScriptTemplate[T <: Script, L <: ScriptLines]:
  def lines(text: String, script: T): L
