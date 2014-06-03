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
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.map(_.prepend(fragmentFactory.Text(s)))
    def ^(others: Fragments)        : Fragments     = fragmentFactory.Text(s) ^ others
    def ^(others: Seq[Fragment])    : Fragments     = ^(Fragments(others:_*))
    def ^(other: Fragment)          : Fragments     = s ^ Fragments(other)
    def ^(other: String)            : Fragments     = s ^ fragmentFactory.Text(other)
  }

  implicit class appendToFragment(f: Fragment) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure  = structure.map(_.prepend(f))
    def ^(others: Fragments)        : Fragments      = Fragments(Fragments(f).contents ++ others.contents)
    def ^(others: Seq[Fragment])    : Fragments      = ^(Fragments(others:_*))
    def ^(other: Fragment)          : Fragments      = Fragments(f, other)
    def ^(other: String)            : Fragments      = f ^ fragmentFactory.Text(other)
  }

  implicit class appendToFragments(fs: Fragments) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.map(_.prepend(fs))
    def ^(others: Fragments)        : Fragments     = fs.append(others)
    def ^(others: Seq[Fragment])    : Fragments     = ^(Fragments(others:_*))
    def ^(other: Fragment)          : Fragments     = fs.append(other)
    def ^(other: String)            : Fragments     = fs ^ fragmentFactory.Text(other)
  }

  implicit class appendToArguments(args: Arguments) {
    def ^(other: Arguments)         : Arguments = args.overrideWith(other)
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.copy(arguments = args)
    def ^(header: SpecHeader)       : SpecStructure = SpecStructure(header, args, Fragments())
    def ^(others: Fragments)        : SpecStructure = SpecStructure(SpecHeader(specClass = outer.getClass), args, others)
    def ^(others: Seq[Fragment])    : SpecStructure = ^(Fragments(others:_*))
    def ^(other: Fragment)          : SpecStructure = args ^ Fragments(other)
    def ^(other: String)            : SpecStructure = args ^ fragmentFactory.Text(other)
  }

  implicit class appendToSpecHeader(header: SpecHeader) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.copy(header = header)
    def ^(args: Arguments)          : SpecStructure = SpecStructure(header, args, Fragments())
    def ^(others: Fragments)        : SpecStructure = SpecStructure(header, Arguments(), others)
    def ^(others: Seq[Fragment])    : SpecStructure = ^(Fragments(others:_*))
    def ^(other: Fragment)          : SpecStructure = header ^ Fragments(other)
    def ^(other: String)            : SpecStructure = header ^ fragmentFactory.Text(other)
  }

  implicit class appendToSpecStructure(structure: SpecStructure) {
    def ^(others: Fragments)    : SpecStructure = structure.copy(fragments = structure.fragments.append(others))
    def ^(others: Seq[Fragment]): SpecStructure = ^(Fragments(others:_*))
    def ^(other: String)        : SpecStructure = structure ^ fragmentFactory.Text(other)
    def ^(other: Fragment)      : SpecStructure = structure ^ Fragments(other)
    /** warning: if other contains arguments or a title they will be lost! */
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(other: SpecStructure): SpecStructure     = structure ^ other.fragments
  }

  // allow writing: def is = "my spec".title
  implicit def specHeaderAsStructure(header: SpecHeader): SpecStructure =
    SpecStructure(header, Arguments(), Fragments())

  // allow writing: def is = ""
  implicit def stringAsSpecStructure(s: String): SpecStructure =
    SpecHeader(getClass) ^ s

  // allow writing: def is = ok
  implicit def resultAsSpecStructure[R : AsResult](r: =>R): SpecStructure =
    SpecHeader(getClass) ^ Fragment(NoText, Execution.result(r))

  // allow writing: def is = "test" ! ok
  implicit def fragmentAsSpecStructure(f: Fragment): SpecStructure =
    SpecHeader(getClass) ^ f

  // allow writing: def is = "a" ! ok ^ "b" ! ok
  implicit def fragmentsAsSpecStructure(fs: Fragments): SpecStructure =
    SpecHeader(getClass) ^ fs

  def section(names: String*)   = fragmentFactory.Section(names:_*)
  def asSection(names: String*) = fragmentFactory.AsSection(names:_*)
  def tag(names: String*)       = fragmentFactory.Tag(names:_*)
  def taggedAs(names: String*)  = fragmentFactory.TaggedAs(names:_*)

  /** shortcut to add tag more quickly when rerunning failed tests */
  private[specs2] def xtag = tag("x")
  /** shortcut to add section more quickly when rerunning failed tests */
  private[specs2] def xsection = section("x")

}

object FragmentDsl extends FragmentDsl
