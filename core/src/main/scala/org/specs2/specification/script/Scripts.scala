package org.specs2
package specification
package script

/**
 * The Scripts trait builds fragments based on Script objects.
 *
 * When the script starts, a section tag is inserted and when it ends another one as well.
 * Also when the scripts ends, it is passed the previous text for analysis to extract new fragments
 *
 */
trait Scripts extends Tags { outer: FragmentsBuilder =>

  /**
   * a sequence of GWT steps can be inserted in a specification to delimit
   * pieces of text to interpret. The "given/when" steps create execute.Step objects while the "then" steps create Examples
   *
   * The whole sequence also creates one tagged section with the title of the sequence
   */
  implicit def scriptIsSpecPart(script: Script): SpecPart = new SpecPart {
    def append(fs: Fragments, text: String, expression: String) = {
      if (script.isStart) fs append section(script.title) append Text(text)
      else                fs append (script.fragments(text) append section(script.title))
    }
  }
}

