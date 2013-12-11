package org.specs2
package specification

import main.Arguments

/**
 * Fragments can be chained with the ^ method
 */
class FragmentsFragment(fs: =>Fragments) {
  lazy val fragments = fs

  def ^(t: String) = fragments add Text(t)
  def ^(f: Fragment) = f match {
    case s: SpecStart => (fragments specTitleIs s.specName).overrideArgs(s.arguments)
    case _            => fragments add f
  }
  def ^(other: Seq[Fragment]) = fragments add other
  def ^(other: Seq[Fragments], dummy: Int = 0) = fragments add other.flatMap(_.fragments)

  def ^(other: Fragments) = {
    other match {
      case Fragments(t, m, a, Linked(Some(l), so, h))    => fragments add other.fragments
      case Fragments(Some(t), m, a, Linked(None, so, h)) => (fragments add other.middle).specTitleIs(t).overrideArgs(a)
      case Fragments(None, m, a, Linked(None, so, h))    => (fragments add other.middle).overrideArgs(a)
      case _                                             =>  fragments add other.middle
    }
  }

  def ^(other: FragmentsFragment) = fragments add other.fragments
  def ^(a: Arguments)             = fragments add a

}

