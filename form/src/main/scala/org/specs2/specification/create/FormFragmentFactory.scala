package org.specs2
package specification
package create

import control.ImplicitParameters.ImplicitParam
import core._
import form._
import text.NotNullStrings._
import execute._

/**
 * Factory for creating Form fragments
 */
trait FormFragmentFactory {
  def FormFragment(form: =>Form): Fragment
  def FormFragment(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragment
}

/**
 * Default implementation for the FormFragment Factory
 */
trait DefaultFormFragmentFactory extends FormFragmentFactory {
  def FormFragment(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragment = addForm(aForm.form)

  def FormFragment(aForm: =>Form): Fragment = addForm(aForm)

  private def addForm(aForm: =>Form): Fragment = {
    lazy val form: Form =
      try aForm.executeForm
      catch { case e: Exception =>
        Form("Initialisation error").tr(PropCell(Prop("", e.getMessage.notNull, (s: String, t: String) => Error(e))("message")))
      }
    Fragment(FormDescription(() => form), Execution.result(form.result.getOrElse(Success(""))))
  }
}
object DefaultFormFragmentFactory extends DefaultFormFragmentFactory


trait FormFragmentsFactory {
  protected def formFragmentFactory: FormFragmentFactory = DefaultFormFragmentFactory
}
