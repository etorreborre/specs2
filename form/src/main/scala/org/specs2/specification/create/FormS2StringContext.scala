package org.specs2
package specification
package create

import core._
import form._
import text.Indent._

/**
 * Allow to use forms inside interpolated strings starting with s2 in order to build the specification content
 */
trait FormS2StringContext extends S2StringContext { this: FormFragmentsFactory =>
  private val factory = fragmentFactory

  private val formFactory = formFragmentFactory
  import formFactory._

  implicit def formIsInterpolatedFragment(f: =>Form): InterpolatedFragment = new InterpolatedFragment {
    override def append(fs: Fragments, text: String, start: Location, end: Location, expression: String): Fragments = {
      val formFragment = FormFragment(f.executeForm).setLocation(end)

      fs append factory.text(text).setLocation(start) append formFragment.updateDescription {
        case fd: FormDescription => fd.indent(lastLineIndentation(text))
        case _                   => formFragment.description
      }
    }
  }

  implicit def toFormIsInterpolatedFragment(f: { def form: Form}): InterpolatedFragment = formIsInterpolatedFragment(f.form)

}

