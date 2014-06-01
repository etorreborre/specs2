package org.specs2
package specification
package dsl

import main.Arguments
import execute.AsResult
import control.ImplicitParameters._
import specification.core._
import specification.create.{FragmentsFactory, DelegatedFragmentFactory}

trait FragmentDsl extends DelegatedFragmentFactory with TitleDsl with ExampleDsl with LinkDsl { outer =>

  implicit def fragmentToFragments(f: Fragment): Fragments =
    Fragments(f)

  implicit class appendToString(s: String) {
    def ^(structure: SpecStructure): SpecStructure = structure.map(_.prepend(fragmentFactory.Text(s)))
    def ^(others: Fragments): Fragments = fragmentFactory.Text(s) ^ others
    def ^(other: Fragment)  : Fragments = s ^ Fragments(other)
    def ^(other: String)    : Fragments = s ^ fragmentFactory.Text(other)
  }

  implicit class appendToFragment(f: Fragment) {
    def ^(structure: SpecStructure): SpecStructure = structure.map(_.prepend(f))
    def ^(others: Fragments): Fragments = Fragments(Fragments(f).contents ++ others.contents)
    def ^(other: Fragment)  : Fragments = Fragments(f, other)
    def ^(other: String)    : Fragments = f ^ fragmentFactory.Text(other)
  }

  implicit class appendToFragments(fs: Fragments) {
    def ^(structure: SpecStructure): SpecStructure = structure.map(_.prepend(fs))
    def ^(others: Fragments): Fragments = fs.append(others)
    def ^(other: Fragment)  : Fragments = fs.append(other)
    def ^(other: String)    : Fragments = fs ^ fragmentFactory.Text(other)
  }

  implicit class appendToArguments(args: Arguments) {
    def ^(structure: SpecStructure): SpecStructure = structure.copy(arguments = args)
    def ^(header: SpecHeader)      : SpecStructure = SpecStructure(header, args, Fragments())
    def ^(others: Fragments)       : SpecStructure = SpecStructure(SpecHeader(specClass = outer.getClass), args, others)
    def ^(other: Fragment)         : SpecStructure = args ^ Fragments(other)
    def ^(other: String)           : SpecStructure = args ^ fragmentFactory.Text(other)
  }

  implicit class appendToSpecHeader(header: SpecHeader) {
    def ^(structure: SpecStructure): SpecStructure = structure.copy(header = header)
    def ^(args: Arguments)         : SpecStructure = SpecStructure(header, args, Fragments())
    def ^(others: Fragments)       : SpecStructure = SpecStructure(header, Arguments(), others)
    def ^(other: Fragment)         : SpecStructure = header ^ Fragments(other)
    def ^(other: String)           : SpecStructure = header ^ fragmentFactory.Text(other)
  }

  implicit class appendToSpecStructure(structure: SpecStructure) {
    def ^(others: Fragments)    : SpecStructure = structure.copy(fragments = structure.fragments.append(others))
    def ^(other: String)        : SpecStructure = structure ^ fragmentFactory.Text(other)
    def ^(other: Fragment)      : SpecStructure = structure ^ Fragments(other)
    /** warning: if other contains arguments or a title they will be lost! */
    def ^(other: SpecStructure) : SpecStructure = structure ^ other.fragments
  }

}

object FragmentDsl extends FragmentDsl
