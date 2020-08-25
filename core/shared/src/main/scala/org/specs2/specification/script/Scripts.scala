package org.specs2
package specification
package script

import scala.quoted._
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
  /**
   * a sequence of GWT steps can be inserted in a specification to delimit
   * pieces of text to interpret. The "given/when" steps create execute.Step objects while the "then" steps create Examples
   *
   * The whole sequence also creates one tagged section with the title of the sequence
   */
  implicit inline def scriptIsInterpolatedFragment(inline script: Script): Interpolated =
    ${Scripts.createInterpolatedFragment('{script}, '{outer.fragmentFactory})}
}

object Scripts:

  def createInterpolatedFragment(script: Expr[Script], factory: Expr[FragmentFactory])(using qctx: QuoteContext): Expr[Interpolated] =
    import qctx.tasty._
    '{ new Interpolated {
         def prepend(text: String): Fragments =
           if ($script.isStart)
             Fragments(${factory}.section(${script}.title)).append(${factory}.text(text))
           else
             ${script}.fragments(text).toFragments.append(${factory}.asSection(${script}.title))
       }}
