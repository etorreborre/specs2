package org.specs2
package specification
package script

import core.Fragments

/**
 * A Script is responsible for analysing a piece of text an creating a sequence of fragments.
 *
 * It usually uses a ScriptTemplate specifying how to parse the text into block of lines that the Script
 * knows how to transform to fragments.
 *
 * For example a GWT script (a `Scenario`) stores functions to create Given/When/Then steps and examples and the
 * LastLinesScriptTemplate extract the last lines of a piece of text and divides them into blocks of Given/When/Then lines
 * based on the number of steps in the Scenario.
 *
 */
trait Script {
  /** @return the title of the script */
  def title: String
  /** create fragments corresponding on this sequence based on a piece of text */
  def fragments(text: String): Fragments

  /** @return true if this object marks the beginning of the script */
  def isStart: Boolean
}

/**
 * set of lines returned by a ScriptTemplate
 */
trait ScriptLines

/**
 * A ScriptTemplate parses some text to create ScriptLines that the associated script knows how to translate to Fragments.
 *
 * For example a script.Specification has a Script which takes text and asks the `BulletedExamplesTemplate` to return
 * FragmentsScriptLines containing Text fragments for normal text and Examples for text that's starting with `+`
 */
trait ScriptTemplate[T <: Script, L <: ScriptLines] {
  def lines(text: String, script: T): L
}

