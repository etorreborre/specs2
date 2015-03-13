package org.specs2
package guide
package matchers

import form._

object MatcherCards extends Cards {
  def title = "Specification Matchers"
  def cards = Seq(
    StringMatchers,
    TraversableMatchers,
    NumericMatchers,
    OptionEitherMatchers,
    TryMatchers,
    FutureMatchers,
    ExceptionMatchers,
    MapMatchers,
    AnyMatchers)
}
