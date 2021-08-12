package org.specs2
package specification
package create

import control.ImplicitParameters.ImplicitParam
import core.*
import form.*
import text.NotNullStrings.*
import execute.*
import control.Exceptions.*
import scala.reflect.Selectable.reflectiveSelectable
import FormsBuilder.{given, *}

/** Factory for creating Form fragments
  */
trait FormFragmentFactory:
  def FormFragment[T: HasForm](aForm: =>T): Fragment

/** Default implementation for the FormFragment Factory
  */
trait DefaultFormFragmentFactory extends FormFragmentFactory:

  def FormFragment[T: HasForm](aForm: =>T): Fragment =
    addForm(aForm.form)

  private def addForm(aForm: =>Form): Fragment =
    lazy val form: Form =
      tryOr(aForm.executeForm) { t =>
        Form("Initialisation error").tr(
          PropCell(prop("", t.getMessage.notNull, (_: String, _: String) => Error(t)).apply("message"))
        )
      }

    Fragment(FormDescription(() => form), Execution.result(form.result.getOrElse(Success(""))))

object DefaultFormFragmentFactory extends DefaultFormFragmentFactory

trait FormFragmentsFactory:
  protected def formFragmentFactory: FormFragmentFactory =
    DefaultFormFragmentFactory
