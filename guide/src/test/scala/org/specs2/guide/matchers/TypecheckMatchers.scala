package org.specs2
package guide
package matchers

import scalaz._, Scalaz._
import org.specs2.execute._, Typecheck._
import specification.core._

object TypecheckMatchers extends UserGuideCard {
  def title = "Typecheck"
  def text =  s2"""

### Typecheck matchers

Some behaviours can be encoded with the type system and you just want to check that a given expression will typecheck: ${snippet{
import org.specs2.execute._, Typecheck._
import org.specs2.matcher.TypecheckMatchers._

typecheck {
    """
  // there must be a Monoid instance for Text
  Monoid[Text].zero
    """
} must succeed
}}

You might also want to check that another expression will fail to typecheck: ${snippet{
// 8<--
import org.specs2.matcher.TypecheckMatchers._
// 8<--

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

#### Typechecked

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

#### Interpolated string

Another way of checking if a piece of code typechecks is to use the `tc` string interpolator: ${snippet{
  tc" 1 must_== 1"
}}

The code above uses a macros which will

  - typecheck the expression
  - replace it with the corresponding code if the expression typechecks ok
  - otherwise it will create a `Typechecked` object containing the reason for the failure

Note that this interpolator can *not* take variables at the moment (so it doesn't interpolate much :-))

#### Typechecking vs parsing

With either the `typecheck` method or the `tc` string interpolator, there might be parse errors. If that's the case they will be raised at compile time because it is likely to be something which you want to fix right way. If you want parse errors to be raised during runtime only you need to use the `parseAndTypecheck` method and the `ptc` string interpolator.

""" ^ br ^ Fragments.foreach(Seq(tc1, tc2, tc3))(_ ^ br)

  import org.specs2.matcher.TypecheckMatchers._

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