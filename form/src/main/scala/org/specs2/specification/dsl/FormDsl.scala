package org.specs2
package specification
package dsl

import main.Arguments
import core._
import form._
import create._
import control.ImplicitParameters.ImplicitParam

trait FormDsl extends FragmentsDsl with FormFragmentsFactory {
  private val factory = formFragmentFactory

  implicit class appendFormToString(s: String) extends appendToString(s) {
    def ^(form: =>Form)                                          : Fragments = appendToString(s) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments = appendToString(s) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToFragment(f: Fragment) extends appendToFragment(f) {
    def ^(form: =>Form)                                          : Fragments = appendToFragment(f) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments = appendToFragment(f) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToFragments(fs: Fragments) extends appendToFragments(fs) {
    def ^(form: =>Form)                                          : Fragments = appendToFragments(fs) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): Fragments = appendToFragments(fs) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToArguments(args: Arguments) extends appendToArguments(args) {
    def ^(form: =>Form)                                          : SpecStructure = appendToArguments(args) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = appendToArguments(args) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToSpecHeader(header: SpecHeader) extends appendToSpecHeader(header) {
    def ^(form: =>Form)                                          : SpecStructure = appendToSpecHeader(header) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = appendToSpecHeader(header) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToSpecStructure(structure: SpecStructure) extends appendToSpecStructure(structure) {
    def ^(form: =>Form)                                          : SpecStructure = appendToSpecStructure(structure) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = appendToSpecStructure(structure) ^ factory.FormFragment(aForm)(p)
  }
}
