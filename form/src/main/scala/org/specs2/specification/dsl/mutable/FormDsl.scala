package org.specs2
package specification
package dsl
package mutable

import control.ImplicitParameters.ImplicitParam
import core.Fragment
import form._
import create._
import org.specs2.control.Use
import scala.reflect.Selectable.reflectiveSelectable

/**
 * Dsl for creating Forms in a mutable specification
 */
trait FormDsl extends FragmentBuilder with FormFragmentFactory:
  outer: FormDsl =>

  def insert(aForm: =>Form): Fragment =
    addFragment(FormFragment(aForm))

  def insert(aForm: =>HasForm)(implicit p: ImplicitParam): Fragment =
    Use.ignoring(p)(insert(aForm.form))

  implicit class insertForm(aForm: => Form):
    def insert = outer.insert(aForm)

  implicit class insertFormHolder(aForm: =>HasForm):
    def insert = outer.insert(aForm)
