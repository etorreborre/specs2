package org.specs2
package guide
package matchers

import fp.syntax.*
import fp.*
import execute.*, Typecheck.*
import matcher.TypecheckMatchers.*
import specification.core.*

object TypecheckMatchers extends UserGuideCard:
  def title = "Typecheck"
  def text = s2"""

### Typecheck matchers

Some behaviours can be encoded with the type system and you just want to check that a given expression will typecheck: ${snippet{
import org.specs2.execute.*, Typecheck.*
import org.specs2.matcher.TypecheckMatchers.*
typecheck {
  """
  // there must be a Monoid instance for Text
  Monoid[Text].zero
  """
} must succeed
}}

You might also want to check that another expression will fail to typecheck: ${snippet{
typecheck {
  """
  // there must be not a Monoid instance for Plane
  Monoid[Plane].zero
  """
} must not(succeed)

typecheck {
  """
  // there must be not a Monoid instance for Plane
  Monoid[Plane].zero
  """
} must failWith("no implicit argument of type org.specs2.fp.Monoid\\[org.specs2.guide.matchers.Plane\\] was found")
}}


#### TypecheckResult

Note that you actually don't have to use the `succeed` matcher because `typecheck` returns a `TypecheckResult` object which has an `AsResult` instance: ${snippet {
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

"""

case class Text(s: String)

object Text:

  given Monoid[Text] with
    def zero = Text("")
    def append(t1: Text, t2: =>Text) = Text(t1.s+t2.s)


case class Plane()
