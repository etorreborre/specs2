package org.specs2
package specification
package dsl

import main.Arguments
import core._
import form._
import create._
import control.ImplicitParameters._

trait FormDsl extends FragmentsDsl with SpecStructureDsl with FormFragmentsFactory {
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
    def ^(form: =>Form)(implicit p1: ImplicitParam1)             : SpecStructure = appendToArguments(args) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = appendToArguments(args) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToSpecHeader(header: SpecHeader) extends appendSpecStructureToSpecHeader(header) {
    def ^(form: =>Form)(implicit p1: ImplicitParam1)             : SpecStructure = appendSpecStructureToSpecHeader(header) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = appendSpecStructureToSpecHeader(header) ^ factory.FormFragment(aForm)(p)
  }

  implicit class appendFormToSpecStructure(structure: SpecStructure) extends appendSpecStructureToSpecStructure(structure) {
    def ^(form: =>Form)(implicit p1: ImplicitParam1)             : SpecStructure = appendSpecStructureToSpecStructure(structure) ^ factory.FormFragment(form)
    def ^(aForm: =>{ def form: Form })(implicit p: ImplicitParam): SpecStructure = appendSpecStructureToSpecStructure(structure) ^ factory.FormFragment(aForm)(p)
  }
}
