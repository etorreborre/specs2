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

  trait ToSpecStructure[T1, T2]:
    def toSpecStructure(t1: T1, t2: =>T2): SpecStructure

  given ToSpecStructure[String, SpecStructure]:
    def toSpecStructure(s: String, structure: =>SpecStructure): SpecStructure =
      structure.map(_.prepend(fragmentFactory.text(s)))

  given ToSpecStructure[String, SpecificationStructure]:
    def toSpecStructure(s: String, spec: =>SpecificationStructure): SpecStructure =
      summon[ToSpecStructure[String, SpecStructure]].toSpecStructure(s, spec.is)

  given ToSpecStructure[Fragment, SpecStructure]:
    def toSpecStructure(f: Fragment, structure: =>SpecStructure): SpecStructure =
      structure.map(_.prepend(f))

  given ToSpecStructure[Fragment, SpecificationStructure]:
    def toSpecStructure(f: Fragment, spec: =>SpecificationStructure): SpecStructure =
      summon[ToSpecStructure[Fragment, SpecStructure]].toSpecStructure(f, spec.is)

  given ToSpecStructure[Fragment, Arguments]:
    def toSpecStructure(f: Fragment, arguments: =>Arguments): SpecStructure =
      summon[ToSpecStructure[SpecStructure, Arguments]].toSpecStructure(f, arguments)

  given ToSpecStructure[SpecHeader, SpecificationStructure]:
    def toSpecStructure(header: SpecHeader, spec: =>SpecificationStructure): SpecStructure =
      summon[ToSpecStructure[SpecHeader, SpecStructure]].toSpecStructure(header, spec.is)

  given ToSpecStructure[SpecHeader, SpecStructure]:
    def toSpecStructure(header: SpecHeader, structure: =>SpecStructure): SpecStructure =
      structure.copy(header = header)

  given ToSpecStructure[SpecHeader, Arguments]:
    def toSpecStructure(header: SpecHeader, arguments: =>Arguments): SpecStructure =
      SpecStructure(header, arguments)

  given ToSpecStructure[SpecHeader, Fragments]:
    def toSpecStructure(header: SpecHeader, fragments: =>Fragments): SpecStructure =
      SpecStructure.create(header, Arguments(), fragments)

  given ToSpecStructure[SpecHeader, Seq[Fragment]]:
    def toSpecStructure(header: SpecHeader, fragments: =>Seq[Fragment]): SpecStructure =
      SpecStructure.create(header, Arguments(), Fragments(fragments:_*))

  given ToSpecStructure[SpecHeader, Fragment]:
    def toSpecStructure(header: SpecHeader, fragment: =>Fragment): SpecStructure =
      SpecStructure.create(header, Arguments(), Fragments(fragment))

  given ToSpecStructure[SpecHeader, String]:
    def toSpecStructure(header: SpecHeader, s: =>String): SpecStructure =
      SpecStructure.create(header, Arguments(), Fragments(fragmentFactory.text(s)))

  given ToSpecStructure[SpecStructure, Fragments]:
    def toSpecStructure(structure: SpecStructure, fragments: =>Fragments): SpecStructure =
      structure.copy(lazyFragments = () => structure.fragments.append(fragments))

  given ToSpecStructure[SpecStructure, Seq[Fragment]]:
    def toSpecStructure(structure: SpecStructure, fragments: =>Seq[Fragment]): SpecStructure =
      structure.copy(lazyFragments = () => structure.fragments.append(Fragments(fragments:_*)))

  given ToSpecStructure[SpecStructure, String]:
    def toSpecStructure(structure: SpecStructure, s: =>String): SpecStructure =
      structure.copy(lazyFragments = () => structure.fragments.append(Fragments(fragmentFactory.text(s))))

  given ToSpecStructure[SpecStructure, Fragment]:
    def toSpecStructure(structure: SpecStructure, f: =>Fragment): SpecStructure =
      structure.copy(lazyFragments = () => structure.fragments.append(Fragments(f)))

  given ToSpecStructure[SpecStructure, SpecStructure]:
    def toSpecStructure(structure1: SpecStructure, structure2: =>SpecStructure): SpecStructure =
      structure1.copy(arguments = structure1.arguments.overrideWith(structure2.arguments)).append(structure2.fragments)

  given ToSpecStructure[SpecStructure, SpecificationStructure]:
    def toSpecStructure(structure: SpecStructure, spec: =>SpecificationStructure): SpecStructure =
      summon[ToSpecStructure[SpecStructure, SpecStructure]].toSpecStructure(structure, spec.is)

  given ToSpecStructure[SpecStructure, Arguments]:
    def toSpecStructure(structure: SpecStructure, arguments: =>Arguments): SpecStructure =
      structure.copy(arguments = structure.arguments.overrideWith(arguments))

  implicit class appendSpecStructure[T1, T2](t1: T1)(using tss: ToSpecStructure[T1, T2]):
    def ^(t2: =>T2): SpecStructure =
      tss.toSpecStructure(t1, t2)

  // allow writing: def is = "my spec".title
  given Conversion[SpecHeader, SpecStructure]:
    def apply(header: SpecHeader): SpecStructure =
      SpecStructure(header)

  // allow writing: def is = ""
  given Conversion[String, SpecStructure]:
    def apply(s: String): SpecStructure =
      SpecStructure.create(SpecHeader(outer.getClass), Arguments(), Fragments(fragmentFactory.text(s)))

  // allow writing: def is = "test" ! ok
  given Conversion[Fragment, SpecStructure]:
    def apply(f: Fragment): SpecStructure =
      SpecStructure.create(SpecHeader(outer.getClass), Arguments(), Fragments(f))

  given Conversion[SpecStructure, Fragments]:
    def apply(spec: SpecStructure): Fragments =
      spec.fragments

private[specs2]
trait SpecStructureDslLowImplicits:
  // allow writing: def is = ok
  implicit def resultAsSpecStructure[R : AsResult](r: =>R): SpecStructure =
    SpecStructure.create(SpecHeader(getClass), Arguments(), Fragments(Fragment(NoText, Execution.result(r))))

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
