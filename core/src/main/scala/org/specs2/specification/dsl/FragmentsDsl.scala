package org.specs2
package specification
package dsl

import main.Arguments
import execute.AsResult
import core._
import create._
import scalaz.syntax.std.vector._

trait FragmentsDsl extends FragmentsFactory with TitleDsl with ExampleDsl with LinkDsl with TagsDsl with ActionDsl { outer =>
  implicit def fragmentToFragments(f: Fragment): Fragments =
    Fragments(f)

  implicit class appendToString(s: String) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.map(_.prepend(fragmentFactory.text(s)))
    def ^(others: Fragments)        : Fragments     = fragmentFactory.text(s) ^ others
    def ^(others: Seq[Fragment])    : Fragments     = ^(Fragments(others:_*))
    def ^(other: Fragment)          : Fragments     = s ^ Fragments(other)
    def ^(other: String)            : Fragments     = s ^ fragmentFactory.text(other)
  }

  implicit class appendToFragment(f: Fragment) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure  = structure.map(_.prepend(f))
    def ^(others: Fragments)        : Fragments      = Fragments(Fragments(f).contents ++ others.contents)
    def ^(others: Seq[Fragment])    : Fragments      = ^(Fragments(others:_*))
    def ^(other: Fragment)          : Fragments      = Fragments(f, other)
    def ^(other: String)            : Fragments      = f ^ fragmentFactory.text(other)
  }

  implicit class appendToFragments(fs: Fragments) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.map(_.prepend(fs))
    def ^(others: Fragments)        : Fragments     = fs.append(others)
    def ^(others: Seq[Fragment])    : Fragments     = ^(Fragments(others:_*))
    def ^(other: Fragment)          : Fragments     = fs.append(other)
    def ^(other: String)            : Fragments     = fs ^ fragmentFactory.text(other)
  }

  implicit class appendToArguments(args: Arguments) {
    def ^(other: Arguments)         : Arguments = args.overrideWith(other)
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.copy(arguments = args)
    def ^(header: SpecHeader)       : SpecStructure = SpecStructure(header, args)
    def ^(others: =>Fragments)        : SpecStructure = SpecStructure(SpecHeader(specClass = outer.getClass), args, () => others)
    def ^(others: Seq[Fragment])    : SpecStructure = ^(Fragments(others:_*))
    def ^(other: Fragment)          : SpecStructure = args ^ Fragments(other)
    def ^(other: String)            : SpecStructure = args ^ fragmentFactory.text(other)
  }

  implicit class appendToSpecHeader(header: SpecHeader) {
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(structure: SpecStructure) : SpecStructure = structure.copy(header = header)
    def ^(args: Arguments)          : SpecStructure = SpecStructure(header, args)
    def ^(others: =>Fragments)      : SpecStructure = SpecStructure.create(header, Arguments(), others)
    def ^(others: Seq[Fragment])    : SpecStructure = ^(Fragments(others:_*))
    def ^(other: Fragment)          : SpecStructure = header ^ Fragments(other)
    def ^(other: String)            : SpecStructure = header ^ fragmentFactory.text(other)
  }

  implicit class appendToSpecStructure(structure: SpecStructure) {
    def ^(others: Fragments)    : SpecStructure = structure.copy(lazyFragments = () => structure.fragments.append(others))
    def ^(others: Seq[Fragment]): SpecStructure = ^(Fragments(others:_*))
    def ^(other: String)        : SpecStructure = structure ^ fragmentFactory.text(other)
    def ^(other: Fragment)      : SpecStructure = structure ^ Fragments(other)
    /** warning: if other contains arguments or a title they will be lost! */
    def ^(s: SpecificationStructure): SpecStructure = ^(s.is)
    def ^(other: SpecStructure): SpecStructure     = structure ^ other.fragments
  }

  // allow writing: def is = "my spec".title
  implicit def specHeaderAsStructure(header: SpecHeader): SpecStructure =
    SpecStructure(header)

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
  implicit def fragmentsAsSpecStructure(fs: =>Fragments): SpecStructure =
    SpecHeader(getClass) ^ fs

  implicit def specStructureAsFragments(spec: SpecStructure): Fragments =
    spec.fragments

  /**
   * create a block of new fragments where each of them is separated
   * by a newline and there is a specific offset from the left margin
   */
  def fragmentsBlock(fragments: Seq[Fragment], offset: Int = 2): Fragments = {
    val newLine = Vector(fragmentFactory.break, fragmentFactory.text(" "*offset))
    (newLine ++ fragments.toList)
      .map(Fragments(_))
      .intersperse(Fragments(newLine:_*))
      .reduce(_ append _)
  }

}

object FragmentsDsl extends FragmentsDsl

/**
 * Lightweight Dsl trait with just a few implicits:
 *
 *  - use arguments
 *  - use s2 string directly in "def is = ..."
 */
private[specs2]
trait FragmentsDsl1 extends LinkCreation with TagsDsl with ActionDsl { outer =>
  implicit class appendToArguments(args: Arguments) {
    def ^(fs: =>Fragments): SpecStructure =
      SpecStructure(SpecHeader(specClass = outer.getClass), args, () => fs)
  }

  // allow writing: def is = "a" ! ok ^ "b" ! ok
  implicit def fragmentsAsSpecStructure(fs: =>Fragments): SpecStructure =
    SpecStructure.create(SpecHeader(getClass), fs)
}
