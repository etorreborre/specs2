package org.specs2
package specification
package dsl

import main.Arguments
import core._
import form.{given, _}
import create._
import control.ImplicitParameters._
import org.specs2.control.Use

/**
 * Dsl for creating Forms in an acceptance specification
 */
trait FormDsl extends FragmentsDsl with SpecStructureDsl with FormFragmentsFactory:

  private val factory = formFragmentFactory

  given [T : HasForm]: ToFragments[T] with
    def toFragments(form: T): Fragments =
      factory.FormFragment(form)

  given [T : HasForm, T2](using ToSpecStructure[T2, Fragment]): ToSpecStructure[T, T2] with
    def toSpecStructure(form: T, t2: =>T2): SpecStructure =
      t2 ^ factory.FormFragment(form)

  given appendFormToString[T : HasForm]: ToSpecStructure[String, T] with
    def toSpecStructure(s: String, form: =>T): SpecStructure =
      fragmentFactory.text(s) ^ factory.FormFragment(form)
