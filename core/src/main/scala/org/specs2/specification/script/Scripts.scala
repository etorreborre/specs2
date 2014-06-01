package org.specs2
package specification
package script

import core._
import create._

/**
 * The Scripts trait builds fragments based on Script objects.
 *
 * When the script starts, a section tag is inserted and when it ends another one as well.
 * Also when the scripts ends, it is passed the previous text for analysis to extract new fragments
 *
 */
trait Scripts { outer: FragmentsFactory =>
  private val factory = fragmentFactory

  /**
   * a sequence of GWT steps can be inserted in a specification to delimit
   * pieces of text to interpret. The "given/when" steps create execute.Step objects while the "then" steps create Examples
   *
   * The whole sequence also creates one tagged section with the title of the sequence
   */
  implicit def scriptIsSpecPart(script: Script): InterpolatedPart = new InterpolatedPart {
    def append(fs: Vector[Fragment], text: String, expression: String): Vector[Fragment] = {
      if (script.isStart) fs :+ factory.Section(script.title) :+ factory.Text(text)
      else                fs ++ (script.fragments(text).fragments.toVector :+ factory.AsSection(script.title))
    }
  }
}

