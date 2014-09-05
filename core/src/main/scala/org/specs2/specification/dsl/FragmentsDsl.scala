package org.specs2
package specification
package dsl

import main.Arguments
import execute.AsResult
import core._
import create._
import scalaz.syntax.std.vector._

/**
 * Creation of Fragments with the ^ syntax
 */
trait FragmentsDsl extends FragmentsFactory with AcceptanceDsl1 { outer =>
  implicit def fragmentToFragments(f: Fragment): Fragments =
    Fragments(f)

  implicit class appendToString(s: String) {
    def ^(others: Fragments)        : Fragments     = fragmentFactory.text(s) ^ others
    def ^(others: Seq[Fragment])    : Fragments     = ^(Fragments(others:_*))
    def ^(other: Fragment)          : Fragments     = s ^ Fragments(other)
    def ^(other: String)            : Fragments     = s ^ fragmentFactory.text(other)
  }

  implicit class appendToFragment(f: Fragment) {
    def ^(others: Fragments)        : Fragments      = Fragments(Fragments(f).contents ++ others.contents)
    def ^(others: Seq[Fragment])    : Fragments      = ^(Fragments(others:_*))
    def ^(other: Fragment)          : Fragments      = Fragments(f, other)
    def ^(other: String)            : Fragments      = f ^ fragmentFactory.text(other)
  }

  implicit class appendToFragments(fs: Fragments) {
    def ^(others: Fragments)        : Fragments     = fs.append(others)
    def ^(others: Seq[Fragment])    : Fragments     = ^(Fragments(others:_*))
    def ^(other: Fragment)          : Fragments     = fs.append(other)
    def ^(other: String)            : Fragments     = fs ^ fragmentFactory.text(other)
  }

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

