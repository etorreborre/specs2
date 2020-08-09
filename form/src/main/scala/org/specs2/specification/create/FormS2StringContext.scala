package org.specs2
package specification
package create

import core._
import form._
import text.Indent._
import scala.reflect.Selectable.reflectiveSelectable

/**
 * Allow to use forms inside interpolated strings starting with s2 in order to build the specification content
 */
trait FormS2StringContext extends S2StringContext { this: FormFragmentsFactory =>

  private val formFactory = formFragmentFactory
  import formFactory._

  implicit def formIsInterpolated(f: =>Form): Interpolated = new Interpolated {
    override def append(text: String): Fragments = {
      val formFragment = FormFragment(f.executeForm)

      Fragments(fragmentFactory.text(text)).append(formFragment.updateDescription {
        case fd: FormDescription => fd.indent(lastLineIndentation(text))
        case _ => formFragment.description
      })
    }
  }

  implicit def toFormIsInterpolatedFragment(f: HasForm): Interpolated =
    formIsInterpolated(f.form)

}
