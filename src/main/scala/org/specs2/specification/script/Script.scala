package org.specs2
package specification
package script


trait Script {
  /** @return the title of the script */
  def title: String
  /** create fragments corresponding on this sequence based on a piece of text */
  def fragments(text: String): Fragments

  /** @return true if this object marks the beginning of the script */
  def isStart: Boolean
}

trait ScriptLines {
  def lines: Seq[Lines]
}
trait ScriptTemplate[T <: Script, L <: ScriptLines] {
  def lines(text: String, script: T): L
}

case class Lines(lines: Seq[String])


