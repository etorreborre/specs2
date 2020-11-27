package org.specs2
package specification
package dsl
package mutable

import control.ImplicitParameters._
import core.Fragment
import form.{given, _}
import create._
import org.specs2.control.Use
import scala.reflect.Selectable.reflectiveSelectable

/**
 * Dsl for creating Forms in a mutable specification
 */
trait FormDsl extends FragmentBuilder with FormFragmentFactory:
  outer =>

  def insert[T : HasForm](aForm: =>T)(using nothing: Int = 0): Fragment =
    addFragment(FormFragment(aForm.form))

  extension [T : HasForm](aForm: =>T):
    def insert: Fragment =
      outer.insert(aForm)
