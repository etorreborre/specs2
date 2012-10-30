package org.specs2
package text

import Quote._
import specification.Grouped

class QuoteSpec extends Specification with Grouped { def is =

  "A string can be added as a prefix to another with a separator"                                                       ! g1.e1^
  "but if it is empty the separator will not be displayed"                                                              ! g2.e2

  new g1 {
    e1 := "Warning".prefix(": ", "dangerous") === "Warning: dangerous"
    e2 := "".prefix(": ", "dangerous")        === "dangerous"
  }
}