package org.specs2
package text

import org.specs2.matcher.ContentMatchers
import AnsiColors._

class LineComparisonSpec extends Specification with ContentMatchers { def is = s2"""

 The differences for content matchers are "clipped" by default meaning
 that only differences are reported and some identical lines
 before and after get printed as well  $clipped

"""

  def clipped = {
    val list1 = List(
      "hello",
      "hello",
      "hello",
      "hello",
      "hello",
      "hello",
      "world",
      "how",
      "are",
      "you",
      "today?"
    )

    val list2 = List(
      "hello",
      "hello",
      "hello",
      "hello",
      "hello",
      "hello",
      "universe",
      "world",
      "how",
      "are",
      "you",
      "today?"
    )

    removeColors((list1 must have sameLinesAs(list2)).message) ====
    s"""|the first sequence is not the same as the second sequence
        |      3. hello
        |      4. hello
        |      5. hello
        |      6. hello
        |    - 7. universe
        |      7. world
        |      8. how
        |      9. are
        |      10. you
        |
        |""".stripMargin
  }


}
