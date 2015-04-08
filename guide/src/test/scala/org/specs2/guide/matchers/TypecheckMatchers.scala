package org.specs2
package guide
package matchers

import scalaz._, Scalaz._
import org.specs2.execute._, Typecheck._
import specification.core._

object TypecheckMatchers extends UserGuideCard with matcher.TypecheckMatchers {
  def title = "Typecheck"
  def text =  s2"""
Some behaviours can be encoded with the type system and you just want to check that a given expression will typecheck: ${snippet{
import org.specs2.execute._, Typecheck._

typecheck {
    """
  // there must be a Monoid instance for Text
  Monoid[Text].zero
    """
} must succeed
}}

You might also want to check that another expression will fail to typecheck: ${snippet{
import org.specs2.execute._, Typecheck._

typecheck {
  """
// there must be not a Monoid instance for Plane
Monoid[Plane].zero
  """
} must not succeed

typecheck {
  """
// there must be not a Monoid instance for Plane
Monoid[Plane].zero
  """
} must failWith("could not find implicit value for .* scalaz.Monoid")
}}

Note that you actually don't have to use the `succeed` matcher because `typecheck` returns a `Typechecked` object which has an `AsResult` instance: ${snippet {
"this should typecheck ok" ! typecheck {
"""
// there must be not a Monoid instance for Plane
Monoid[Plane].zero
"""
}
}}

This is also why you can indicate that a block of code must be marked as `Pending` until it typechecks: ${snippet{
typecheck {
  """
// there must be not a Monoid instance for Plane
Monoid[Plane].zero
  """
}.pendingUntilFixed("find a way to make this typecheck!")

}}

""" ^ br ^ Fragments.foreach(Seq(tc1, tc2, tc3))(_ ^ br)

  def tc1 = "typecheck ok" ! {typecheck {
    """
    // there must be a Monoid instance for Text
    Monoid[Text].zero
    """
  } must succeed}

  def tc2 = "typecheck ko" ! {typecheck {
    """
    // there must be not a Monoid instance for Plane
    Monoid[Plane].zero
    """
  } must failWith("could not find implicit value for .* scalaz.Monoid")}

  def tc3 = "typecheck pending" ! typecheck {
    """
// there must be not a Monoid instance for Plane
Monoid[Plane].zero
    """
  }.pendingUntilFixed("find a way to make this typecheck!")
}

case class Text(s: String)
object Text {
  implicit def TextMonoid: Monoid[Text] = new Monoid[Text] {
    def zero = Text("")
    def append(t1: Text, t2: =>Text) = Text(t1.s+t2.s)
  }
}

case class Plane()