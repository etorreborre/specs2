package org.specs2
package specification
package dsl

import main.Arguments
import core._
import form._
import create._
import control.ImplicitParameters.ImplicitParam

trait FormDsl extends FragmentDsl with FormFragmentsFactory {
  private val factory = formFragmentFactory

  implicit class appendFormToString(s: String) {
    def ^(form: =>Form): Fragments = s ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments = s ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToFragment(f: Fragment) {
    def ^(form: =>Form): Fragments = f ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments = f ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToFragments(fs: Fragments) {
    def ^(form: =>Form): Fragments = fs ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments = fs ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToArguments(args: Arguments) {
    def ^(form: =>Form): SpecStructure = args ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = args ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToSpecHeader(header: SpecHeader) {
    def ^(form: =>Form): SpecStructure = header ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = header ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToSpecStructure(structure: SpecStructure) {
    def ^(form: =>Form): SpecStructure = structure ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = structure ^ factory.FormFragment(aForm)(p)
  }
}
