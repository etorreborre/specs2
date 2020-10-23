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
trait SpecStructureDsl extends SpecStructureDsl1 with SpecStructureDslLowImplicits:
  private val outer = this

  extension (s: String)
    def ^(spec: SpecificationStructure): SpecStructure =
      extension_^(s)(spec.is)

    def ^(structure: SpecStructure) : SpecStructure =
      structure.map(_.prepend(fragmentFactory.text(s)))

  extension (f: Fragment)
    def ^(s: SpecificationStructure): SpecStructure =
      f ^ s.is

    def ^(structure: SpecStructure): SpecStructure =
      structure.map(_.prepend(f))

    def ^(arguments: Arguments): SpecStructure =
      (f : SpecStructure) ^ arguments

  extension (header: SpecHeader)
    def ^(s: SpecificationStructure): SpecStructure =
      header ^ s.is

    def ^(structure: SpecStructure) : SpecStructure =
      structure.copy(header = header)

    def ^(args: Arguments): SpecStructure =
      SpecStructure(header, args)

    def ^(others: =>Fragments): SpecStructure =
      SpecStructure.create(header, Arguments(), others)

    def ^(others: Seq[Fragment]): SpecStructure =
      header ^ Fragments(others:_*)

    def ^(other: Fragment): SpecStructure =
      header ^ Fragments(other)

    def ^(other: String): SpecStructure =
      header ^ fragmentFactory.text(other)

  extension (structure: SpecStructure)
    def ^(others: Fragments): SpecStructure =
      structure.copy(lazyFragments = () => structure.fragments.append(others))

    def ^(others: Seq[Fragment]): SpecStructure =
      structure ^ Fragments(others:_*)

    def ^(other: String): SpecStructure =
      structure ^ fragmentFactory.text(other)

    def ^(other: Fragment): SpecStructure =
      structure ^ Fragments(other)

    /** warning: if other contains arguments or a title they will be lost! */
    def ^(s: SpecificationStructure): SpecStructure =
      structure ^ s.is

    def ^(other: SpecStructure): SpecStructure =
      structure.copy(arguments = structure.arguments.overrideWith(other.arguments)) ^ other.fragments

    def ^(arguments: Arguments): SpecStructure =
      structure.copy(arguments = structure.arguments.overrideWith(arguments))

  // allow writing: def is = "my spec".title
  given Conversion[SpecHeader, SpecStructure]:
    def apply(header: SpecHeader): SpecStructure =
      SpecStructure(header)

  // allow writing: def is = ""
  given Conversion[String, SpecStructure]:
    def apply(s: String): SpecStructure =
      SpecHeader(getClass) ^ s

  // allow writing: def is = "test" ! ok
  given Conversion[Fragment, SpecStructure]:
    def apply(f: Fragment): SpecStructure =
      SpecHeader(getClass) ^ f

  given Conversion[SpecStructure, Fragments]:
    def apply(spec: SpecStructure): Fragments =
      spec.fragments

private[specs2]
trait SpecStructureDslLowImplicits:
  // allow writing: def is = ok
  implicit def resultAsSpecStructure[R : AsResult](r: =>R): SpecStructure =
    SpecStructure.create(SpecHeader(getClass), Arguments(), Fragments(Fragment(NoText, Execution.result(r))))

  extension (fs: =>Fragments)
    def ^(s: SpecificationStructure): SpecStructure =
      fs ^ s.is

    def ^(structure: SpecStructure): SpecStructure =
      structure.map(_.prepend(fs))

private[specs2]
trait SpecStructureDsl1 extends FragmentsFactory:
  private val outer = this

  extension (args: Arguments)
    def ^(other: Arguments): Arguments =
      args.overrideWith(other)

    def ^(s: SpecificationStructure): SpecStructure =
      args ^ s.is

    def ^(structure: SpecStructure): SpecStructure =
      structure.copy(arguments = args)

    def ^(header: SpecHeader): SpecStructure =
      SpecStructure(header, args)

    def ^(others: =>Fragments): SpecStructure =
      SpecStructure(SpecHeader(specClass = outer.getClass), args, () => others)

    def ^(others: Seq[Fragment]): SpecStructure =
      args ^ Fragments(others:_*)

    def ^(other: Fragment): SpecStructure =
      args ^ Fragments(other)

    def ^(other: String): SpecStructure =
      args ^ fragmentFactory.text(other)

  // allow writing: def is = "a" ! ok ^ "b" ! ok
  implicit def fragmentsAsSpecStructure(fs: =>Fragments): SpecStructure =
    SpecStructure.create(SpecHeader(getClass), fs)
