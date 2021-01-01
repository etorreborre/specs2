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
trait SpecStructureDsl extends FragmentsFactory:
  private val outer = this

  trait ToSpecStructure[T1, T2]:
    def toSpecStructure(t1: T1, t2: =>T2): SpecStructure

  extension [T1, T2](t1: T1)
    def ^(t2: =>T2)(using tss: ToSpecStructure[T1, T2]): SpecStructure =
      tss.toSpecStructure(t1, t2)

  /** APPENDING TO A STRING */
  given ToSpecStructure[String, Fragment] with
    def toSpecStructure(s: String, fragment: =>Fragment): SpecStructure =
      Fragments(fragmentFactory.text(s), fragment)

  given ToSpecStructure[String, Fragments] with
    def toSpecStructure(s: String, fs: =>Fragments): SpecStructure =
      fs.prepend(fragmentFactory.text(s))

  given ToSpecStructure[String, SpecStructure] with
    def toSpecStructure(s: String, structure: =>SpecStructure): SpecStructure =
      structure.map(_.prepend(fragmentFactory.text(s)))

  given [T <: SpecificationStructure]: ToSpecStructure[String, T] with
    def toSpecStructure(s: String, spec: =>T): SpecStructure =
      summon[ToSpecStructure[String, SpecStructure]].toSpecStructure(s, spec.is)

  /** APPENDING TO A FRAGMENT */
  given ToSpecStructure[Fragment, String] with
    def toSpecStructure(f1: Fragment, s: =>String): SpecStructure =
      Fragments(f1, fragmentFactory.text(s))

  given ToSpecStructure[Fragment, Fragment] with
    def toSpecStructure(f1: Fragment, f2: =>Fragment): SpecStructure =
      Fragments(f1, f2)

  given ToSpecStructure[Fragment, Fragments] with
    def toSpecStructure(f: Fragment, fs: =>Fragments): SpecStructure =
      fs.prepend(f)

  given ToSpecStructure[Fragment, SpecStructure] with
    def toSpecStructure(f: Fragment, structure: =>SpecStructure): SpecStructure =
      structure.map(_.prepend(f))

  given [T <: SpecificationStructure]: ToSpecStructure[Fragment, T] with
    def toSpecStructure(f: Fragment, spec: =>T): SpecStructure =
      summon[ToSpecStructure[Fragment, SpecStructure]].toSpecStructure(f, spec.is)

  given ToSpecStructure[Fragment, Arguments] with
    def toSpecStructure(f: Fragment, arguments: =>Arguments): SpecStructure =
      summon[ToSpecStructure[SpecStructure, Arguments]].toSpecStructure(f, arguments)

  /** APPENDING TO FRAGMENTS */
  given ToSpecStructure[Fragments, String] with
    def toSpecStructure(fs: Fragments, s: =>String): SpecStructure =
      fs.append(fragmentFactory.text(s))

  given ToSpecStructure[Fragments, Fragment] with
    def toSpecStructure(fs: Fragments, f: =>Fragment): SpecStructure =
      fs.append(f)

  given ToSpecStructure[Fragments, Fragments] with
    def toSpecStructure(fs1: Fragments, fs2: =>Fragments): SpecStructure =
      fs1.append(fs2)

  /** APPENDING TO ARGUMENTS */
  given ToSpecStructure[Arguments, String] with
    def toSpecStructure(arguments: Arguments, s: =>String): SpecStructure =
      fragmentFactory.text(s).copy(arguments = arguments)

  given ToSpecStructure[Arguments, Fragment] with
    def toSpecStructure(arguments: Arguments, f: =>Fragment): SpecStructure =
      f.copy(arguments = arguments)

  given ToSpecStructure[Arguments, Fragments] with
    def toSpecStructure(arguments: Arguments, fs: =>Fragments): SpecStructure =
      fs.copy(arguments = arguments)

  given ToSpecStructure[Arguments, SpecHeader] with
    def toSpecStructure(arguments: Arguments, header: =>SpecHeader): SpecStructure =
      SpecStructure.create(header, arguments, Fragments())

  given ToSpecStructure[Arguments, Arguments] with
    def toSpecStructure(arguments1: Arguments, arguments2: =>Arguments): SpecStructure =
      SpecStructure.create(SpecHeader(outer.getClass), arguments1.overrideWith(arguments2), Fragments())

  given ToSpecStructure[Arguments, SpecStructure] with
    def toSpecStructure(arguments: Arguments, spec: =>SpecStructure): SpecStructure =
      spec.copy(arguments = arguments)

  given [T <: SpecificationStructure]: ToSpecStructure[Arguments, T] with
    def toSpecStructure(arguments: Arguments, spec: =>T): SpecStructure =
      spec.is.copy(arguments = arguments)

  /** APPENDING TO A SPEC HEADER */
  given [T <: SpecificationStructure]: ToSpecStructure[SpecHeader, T] with
    def toSpecStructure(header: SpecHeader, spec: =>T): SpecStructure =
      summon[ToSpecStructure[SpecHeader, SpecStructure]].toSpecStructure(header, spec.is)

  given ToSpecStructure[SpecHeader, SpecStructure] with
    def toSpecStructure(header: SpecHeader, structure: =>SpecStructure): SpecStructure =
      structure.copy(header = header)

  given ToSpecStructure[SpecHeader, Arguments] with
    def toSpecStructure(header: SpecHeader, arguments: =>Arguments): SpecStructure =
      SpecStructure(header, arguments)

  given ToSpecStructure[SpecHeader, Fragments] with
    def toSpecStructure(header: SpecHeader, fragments: =>Fragments): SpecStructure =
      SpecStructure.create(header, Arguments(), fragments)

  given ToSpecStructure[SpecHeader, Seq[Fragment]] with
    def toSpecStructure(header: SpecHeader, fragments: =>Seq[Fragment]): SpecStructure =
      SpecStructure.create(header, Arguments(), Fragments(fragments:_*))

  given ToSpecStructure[SpecHeader, Fragment] with
    def toSpecStructure(header: SpecHeader, fragment: =>Fragment): SpecStructure =
      SpecStructure.create(header, Arguments(), Fragments(fragment))

  given ToSpecStructure[SpecHeader, String] with
    def toSpecStructure(header: SpecHeader, s: =>String): SpecStructure =
      SpecStructure.create(header, Arguments(), Fragments(fragmentFactory.text(s)))

  /** APPENDING TO A SPEC STRUCTURE */
  given ToSpecStructure[SpecStructure, Fragments] with
    def toSpecStructure(structure: SpecStructure, fragments: =>Fragments): SpecStructure =
      structure.copy(lazyFragments = () => structure.fragments.append(fragments))

  given ToSpecStructure[SpecStructure, Seq[Fragment]] with
    def toSpecStructure(structure: SpecStructure, fragments: =>Seq[Fragment]): SpecStructure =
      structure.copy(lazyFragments = () => structure.fragments.append(Fragments(fragments:_*)))

  given ToSpecStructure[SpecStructure, String] with
    def toSpecStructure(structure: SpecStructure, s: =>String): SpecStructure =
      structure.copy(lazyFragments = () => structure.fragments.append(Fragments(fragmentFactory.text(s))))

  given ToSpecStructure[SpecStructure, Fragment] with
    def toSpecStructure(structure: SpecStructure, f: =>Fragment): SpecStructure =
      structure.copy(lazyFragments = () => structure.fragments.append(Fragments(f)))

  given ToSpecStructure[SpecStructure, SpecStructure] with
    def toSpecStructure(structure1: SpecStructure, structure2: =>SpecStructure): SpecStructure =
      structure1.copy(arguments = structure1.arguments.overrideWith(structure2.arguments)).append(structure2.fragments)

  given [T <: SpecificationStructure]: ToSpecStructure[SpecStructure, T] with
    def toSpecStructure(structure: SpecStructure, spec: =>T): SpecStructure =
      summon[ToSpecStructure[SpecStructure, SpecStructure]].toSpecStructure(structure, spec.is)

  given ToSpecStructure[SpecStructure, Arguments] with
    def toSpecStructure(structure: SpecStructure, arguments: =>Arguments): SpecStructure =
      structure.copy(arguments = structure.arguments.overrideWith(arguments))

  implicit class appendSpecStructure[T1, T2](t1: T1)(using tss: ToSpecStructure[T1, T2]):
    def ^(t2: =>T2): SpecStructure =
      tss.toSpecStructure(t1, t2)

  // allow writing: def is = "my spec".title
  given Conversion[SpecHeader, SpecStructure] with
    def apply(header: SpecHeader): SpecStructure =
      SpecStructure(header)

  // allow writing: def is = ""
  given Conversion[String, SpecStructure] with
    def apply(s: String): SpecStructure =
      SpecStructure.create(SpecHeader(outer.getClass), Arguments(), Fragments(fragmentFactory.text(s)))

  // allow writing: def is = "test" ! ok
  given Conversion[Fragment, SpecStructure] with
    def apply(f: Fragment): SpecStructure =
      Fragments(f)

  given Conversion[SpecStructure, Fragments] with
    def apply(spec: SpecStructure): Fragments =
      spec.fragments

  // allow writing: def is = "a" ! ok ^ "b" ! ok
  implicit def fragmentsAsSpecStructure(fs: =>Fragments): SpecStructure =
    SpecStructure.create(SpecHeader(getClass), fs)

    // allow writing: def is = ok
  implicit def resultAsSpecStructure[R : AsResult](r: =>R): SpecStructure =
    SpecStructure.create(SpecHeader(getClass), Arguments(), Fragments(Fragment(NoText, Execution.result(r))))
