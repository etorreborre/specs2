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

#### Typechecking parameters

It is possible to tweak the behaviour of the `typecheck` method to allow errors to be reported at compile-time instead of runtime:

 - macro-expansion errors can be reported at compile-time (default behaviour is runtime)
 - implicit errors can be reported at compile-time (default behaviour is runtime)
 - parsing errors can be reported at runtime (default behaviour is compile-time)

Here is how to do it:

    // to get macro errors at compile time
    typecheckWith(macrosAtCompileTime)("badBadMacro")

    // to get macro errors at compile time
    typecheckWith(implicitsAtCompileTime)("Monoid[Plane]")

    // to get parsing errors at run-time
    typecheckWith(parsingAtRuntime)("#$$p6")

    // combine parameters
    typecheckWith(macrosAtCompileTime <| implicitsAtCompileTime)("Monoid[Plane]")

Finally you can also use a string interpolator and pass parameters:
```
  tcw" 1 must_== 1"(parsingAtRuntime)
```

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
