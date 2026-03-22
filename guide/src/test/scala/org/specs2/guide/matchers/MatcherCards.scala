package org.specs2
package guide
package matchers

import form.*

object MatcherCards extends Cards {
  def title = "Specification matchers"
  def cards = Seq(
    StringMatchers,
    IterableMatchers,
    NumericMatchers,
    OptionEitherMatchers,
    TryMatchers,
    FutureMatchers,
    ExceptionMatchers,
    MapMatchers,
    AnyMatchers
  )
}
