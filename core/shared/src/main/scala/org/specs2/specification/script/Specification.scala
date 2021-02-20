package org.specs2
package specification
package script

import core._

/**
 * These classes and traits enrich regular specifications with the possibility to include "scripts" in their definition.
 * (see Scripts.scala)
 */

abstract class Specification extends SpecificationLike

trait SpecificationLike extends org.specs2.SpecificationLike with Scripts

abstract class Spec extends SpecLike

trait SpecLike extends org.specs2.Spec with Scripts

/**
 * List of fragments with utility functions to manipulate it
 * @param fs
 */
case class FragmentsSeq(fs: Vector[Fragment]):

  def toFragments: Fragments =
    Fragments.apply(fs*)

  def map(f: Fragment => Fragment): FragmentsSeq =
    FragmentsSeq(fs map f)

  def append(fragment: Fragment): FragmentsSeq =
    FragmentsSeq(fs :+ fragment)

  def append(other: FragmentsSeq): FragmentsSeq =
    FragmentsSeq(fs ++ other.fs)

  def append(other: Seq[Fragment]): FragmentsSeq =
    FragmentsSeq(fs ++ other.toVector)

  def ++(other: FragmentsSeq): FragmentsSeq =
    append(other)

  def :+(fragment: Fragment): FragmentsSeq =
    FragmentsSeq(fs :+ fragment)

  def compact: FragmentsSeq =
    this

object FragmentsSeq:

  val empty: FragmentsSeq =
    FragmentsSeq(Vector.empty)

  def apply(f: Fragment, fs: Fragment*): FragmentsSeq =
    FragmentsSeq(f +: fs.toVector)
