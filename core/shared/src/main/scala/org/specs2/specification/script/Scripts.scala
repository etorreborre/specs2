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
  implicit def scriptIsInterpolatedFragment(script: Script): InterpolatedFragment = new InterpolatedFragment {
    def append(fs: Fragments, text: String, start: Location, end: Location, expression: String): Fragments = {
      if (script.isStart)
        fs append factory.section(script.title) append factory.text(text).setLocation(start)
      else
        fs.compact.updateFragments { fragments: List[Fragment] =>
            val scriptFragments =
              fragments.lastOption match {
                case Some(f) if Fragment.isText(f) =>
                  Fragments(fragments.dropRight(1):_*) append script.fragments(f.description.show + text).map(_.setLocation(end)).toFragments

                case None =>
                  Fragments(fragments:_*) append script.fragments(text).map(_.setLocation(end)).toFragments

              }
            scriptFragments append factory.asSection(script.title)
          }
    }
  }
}

