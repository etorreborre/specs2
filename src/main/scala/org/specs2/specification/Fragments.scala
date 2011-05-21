package org.specs2
package specification

import execute.Executable
import control.LazyParameters._
import control.LazyParameter
import main.Arguments
import StandardFragments._
import org.specs2.internal.scalaz.Monoid

/**
 * A Fragments object is a list of fragments with a SpecStart and a SpecEnd
 */
class Fragments (val specStart: Option[SpecStart] = None, val middle: Seq[Fragment] = Nil, val specEnd: Option[SpecEnd] = None) {
  def fragments = if (middle.isEmpty) Seq() else (start +: middle :+ end)

  private def append(e: Fragment) = new Fragments(specStart, middle :+ e, specEnd)
  def specTitleIs(s: SpecStart): Fragments = new Fragments(Some(start.withName(s.name)), middle, specEnd)
  def add(e: Fragment): Fragments = append(e)
  def add(fs: Seq[Fragment]): Fragments = new Fragments(specStart, middle ++ fs, specEnd)
  def add(fs: Fragments): Fragments = add(fs.fragments)
  def add(a: Arguments): Fragments = new Fragments(Some(start.withArgs(a)), middle, specEnd)

  def executables: Seq[Executable] = fragments.collect { case e: Executable => e }
  def overrideArgs(args: Arguments) = new Fragments(Some(start.overrideArgs(args)), middle, specEnd)
  def map(function: Fragment => Fragment) = new Fragments(specStart, middle.map(function), specEnd)
  import StandardFragments._
  override def toString = fragments.mkString("\n")

  def arguments = start.arguments
  def start = specStart.getOrElse(SpecStart(""))
  def end = specEnd.getOrElse(SpecEnd("").withName(start.name))
}

/**
 * Utility methods for fragments
 */
object Fragments {
  /**
   * @return a Fragments object containing only a seq of Fragments.
   */
  def createList(fs: Fragment*) = new Fragments(middle = fs)
  /**
   * @return a Fragments object, where the SpecStart might be provided by the passed fragments
   */
  def create(fs: Fragment*) = {
    fs.toList match {
      case (s @ SpecStart(n, a)) :: rest => new Fragments(Some(s), middle = rest)
      case _                             => createList(fs:_*)
    }
  }

  /** @return true if the Fragment is a Text */
  def isText: Function[Fragment, Boolean] = { case Text(_) => true; case _ => false }
  /** @return the text if the Fragment is a Text */
  def isSomeText: PartialFunction[Fragment, Text] = { case t @ Text(_) => t }
  /** @return true if the Fragment is an Example */
  def isExample: Function[Fragment, Boolean] = { case Example(_, _) => true; case _ => false }
  /** @return the example if the Fragment is an Example */
  def isAnExample: PartialFunction[Fragment, Example] = { case e @ Example(_,_) => e }
  /** @return true if the Fragment is a step */
  def isStep: Function[Fragment, Boolean] = { case Step(_) => true; case _ => false }
  /** @return the step if the Fragment is a Step*/
  def isAStep: PartialFunction[Fragment, Step] = { case s @ Step(_) => s }

  /** @return a Fragments object with the appropriate name set on the SpecStart fragment */
  def withSpecStartEnd(fragments: Fragments, name: SpecName): Fragments = {
    val specStart = fragments.start.withName(name)
    new Fragments(Some(specStart), fragments.middle, Some(fragments.end.withName(specStart.name)))
  }
  /**
   * @return a Fragments object with the appropriate name set on the SpecStart fragment
   *
   * That name is derived from the specification structure name
   */
  def withSpecStartEnd(fragments: Fragments, s: SpecificationStructure): Fragments = withSpecStartEnd(fragments, SpecName(s))

  /**
   * Fragments can be added as a monoid
   */
  implicit def fragmentsIsMonoid = new Monoid[Fragments] {
    val zero = new Fragments()
    def append(s1: Fragments, s2: => Fragments) = s1 add s2
  }
}

