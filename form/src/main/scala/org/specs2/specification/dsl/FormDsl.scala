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
    def ^(form: =>Form)                                          : Fragments = appendToString(s) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments = appendToString(s) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToFragment(f: Fragment) {
    def ^(form: =>Form)                                          : Fragments = appendToFragment(f) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments = appendToFragment(f) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToFragments(fs: Fragments) {
    def ^(form: =>Form)                                          : Fragments = appendToFragments(fs) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments = appendToFragments(fs) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToArguments(args: Arguments) {
    def ^(form: =>Form)                                          : SpecStructure = appendToArguments(args) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = appendToArguments(args) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToSpecHeader(header: SpecHeader) {
    def ^(form: =>Form)                                          : SpecStructure = appendToSpecHeader(header) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = appendToSpecHeader(header) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToSpecStructure(structure: SpecStructure) {
    def ^(form: =>Form)                                          : SpecStructure = appendToSpecStructure(structure) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = appendToSpecStructure(structure) ^ factory.FormFragment(aForm)(p)
  }
}
