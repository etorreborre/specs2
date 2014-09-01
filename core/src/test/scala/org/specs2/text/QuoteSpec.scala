package org.specs2
package text

import Quote._
import specification._
import matcher._

class QuoteSpec extends script.Spec with Groups with TypedEqualExpectations { def is = s2"""

 + A string can be added as a prefix to another with a separator
 + but if it is empty the separator will not be displayed
                                                                        """

  "quotes" - new group {
    eg := { "Warning".prefix(": ", "dangerous") === "Warning: dangerous" }
    eg := { "".prefix(": ", "dangerous")        === "dangerous"          }
  }
}