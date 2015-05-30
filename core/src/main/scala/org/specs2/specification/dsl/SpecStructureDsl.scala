package org.specs2
package specification
package dsl

import core._
import main._
import execute.AsResult
import create.FragmentsFactory

/**
 * Creation of SpecStructure with the ^ syntax
 */
trait SpecStructureDsl extends SpecStructureDsl1 with SpecStructureDslLowImplicits { outer =>

  implicit class appendSpecStructureToString(s: String) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.map(_.prepend(fragmentFactory.text(s)))
  }

  implicit class appendSpecStructureToFragment(f: Fragment) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure  = structure.map(_.prepend(f))
    def ^(arguments: Arguments) : SpecStructure  = fragmentAsSpecStructure(f) ^ arguments
  }

  implicit class appendSpecStructureToSpecHeader(header: SpecHeader) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.copy(header = header)
    def ^(args: Arguments)          : SpecStructure = SpecStructure(header, args)
    def ^(others: =>Fragments)      : SpecStructure = SpecStructure.create(header, Arguments(), others)
    def ^(others: Seq[Fragment])    : SpecStructure = ^(Fragments(others:_*))
    def ^(other: Fragment)          : SpecStructure = header ^ Fragments(other)
    def ^(other: String)            : SpecStructure = header ^ fragmentFactory.text(other)
  }

  implicit class appendSpecStructureToSpecStructure(structure: SpecStructure) {
    def ^(others: Fragments)    : SpecStructure = structure.copy(lazyFragments = () => structure.fragments.append(others))
    def ^(others: Seq[Fragment]): SpecStructure = ^(Fragments(others:_*))
    def ^(other: String)        : SpecStructure = structure ^ fragmentFactory.text(other)
    def ^(other: Fragment)      : SpecStructure = structure ^ Fragments(other)
    /** warning: if other contains arguments or a title they will be lost! */
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(other: SpecStructure): SpecStructure     = structure.copy(arguments = structure.arguments.overrideWith(other.arguments)) ^ other.fragments
    def ^(arguments: Arguments): SpecStructure     = structure.copy(arguments = structure.arguments.overrideWith(arguments))
  }

  // allow writing: def is = "my spec".title
  implicit def specHeaderAsStructure(header: SpecHeader): SpecStructure =
    SpecStructure(header)

  // allow writing: def is = ""
  implicit def stringAsSpecStructure(s: String): SpecStructure =
    SpecHeader(getClass) ^ s

  // allow writing: def is = "test" ! ok
  implicit def fragmentAsSpecStructure(f: Fragment): SpecStructure =
    SpecHeader(getClass) ^ f

  implicit def specStructureAsFragments(spec: SpecStructure): Fragments =
    spec.fragments

}

private[specs2]
trait SpecStructureDslLowImplicits {
  // allow writing: def is = ok
  implicit def resultAsSpecStructure[R : AsResult](r: =>R): SpecStructure =
    SpecStructure.create(SpecHeader(getClass), Arguments(), Fragments(Fragment(NoText, Execution.result(r))))

  implicit class appendSpecStructureToFragments(fs: =>Fragments) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.map(_.prepend(fs))
  }
}

private[specs2]
trait SpecStructureDsl1 extends FragmentsFactory { outer =>
  implicit class appendToArguments(args: Arguments) {
    def ^(other: Arguments)         : Arguments = args.overrideWith(other)
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.copy(arguments = args)
    def ^(header: SpecHeader)       : SpecStructure = SpecStructure(header, args)
    def ^(others: =>Fragments)      : SpecStructure = SpecStructure(SpecHeader(specClass = outer.getClass), args, () => others)
    def ^(others: Seq[Fragment])    : SpecStructure = ^(Fragments(others:_*))
    def ^(other: Fragment)          : SpecStructure = args ^ Fragments(other)
    def ^(other: String)            : SpecStructure = args ^ fragmentFactory.text(other)
  }

  // allow writing: def is = "a" ! ok ^ "b" ! ok
  implicit def fragmentsAsSpecStructure(fs: =>Fragments): SpecStructure =
    SpecStructure.create(SpecHeader(getClass), fs)
}

/** deactivate the spec structure Dsl implicits */
trait NoSpecStructureDsl extends SpecStructureDsl {
  override def appendSpecStructureToString(s: String) =
    super.appendSpecStructureToString(s)

  override def appendSpecStructureToFragment(f: Fragment) =
    super.appendSpecStructureToFragment(f)

  override def appendSpecStructureToSpecHeader(header: SpecHeader) =
    super.appendSpecStructureToSpecHeader(header)

  override def appendSpecStructureToSpecStructure(structure: SpecStructure) =
    super.appendSpecStructureToSpecStructure(structure)
}
