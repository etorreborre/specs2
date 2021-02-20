package org.specs2
package specification
package dsl
package mutable

import control.ImplicitParameters.*
import core.Fragment
import form.{given, *}
import create.*
import org.specs2.control.Use
import scala.reflect.Selectable.reflectiveSelectable

/**
 * Dsl for creating Forms in a mutable specification
 */
trait FormDsl extends FragmentBuilder with FormFragmentFactory:
  outer =>

  def insertForm[T : HasForm](aForm: =>T): Fragment =
    addFragment(FormFragment(aForm.form))

  extension [T : HasForm](aForm: =>T)
    def insert: Fragment =
      outer.insertForm(aForm)
