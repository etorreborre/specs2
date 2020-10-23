package org.specs2
package specification
package dsl

import main.Arguments
import core._
import form._
import create._
import control.ImplicitParameters._
import org.specs2.control.Use

/**
 * Dsl for creating Forms in an acceptance specification
 */
trait FormDsl extends FragmentsDsl with SpecStructureDsl with FormFragmentsFactory:
  private val factory = formFragmentFactory

  extension (s: String)
    def ^(form: =>Form): Fragments =
      s ^ factory.FormFragment(form)

    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments =
      s ^ factory.FormFragment(aForm)(p)

  extension (f: Fragment)
    def ^(form: =>Form): Fragments =
      f ^ factory.FormFragment(form)

    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments =
      f ^ factory.FormFragment(aForm)(p)

  extension (fs: Fragments)
    def ^(form: =>Form): Fragments =
      fs ^ factory.FormFragment(form)
      
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments =
      fs ^ factory.FormFragment(aForm)(p)

  extension (args: Arguments)
    def ^(form: =>Form)(implicit p1: ImplicitParam1): SpecStructure =
      Use.ignoring(p1)(args ^ factory.FormFragment(form))

    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure =
      args ^ factory.FormFragment(aForm)(p)

  extension (header: SpecHeader)
    def ^(form: =>Form)(implicit p1: ImplicitParam1) : SpecStructure =
      Use.ignoring(p1)(header ^ factory.FormFragment(form))

    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure =
      header ^ factory.FormFragment(aForm)(p)

  extension (structure: SpecStructure)
    def ^(form: =>Form)(implicit p1: ImplicitParam1): SpecStructure =
      Use.ignoring(p1)(structure ^ factory.FormFragment(form))

    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure =
      structure ^ factory.FormFragment(aForm)(p)
