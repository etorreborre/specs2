package org.specs2
package text

import Quote._
import matcher._

class QuoteSpec extends Spec with TypedEqual { def is = s2"""

normal strings can be quoted $quote1
 A string can be added as a prefix to another with a separator $quote2
 but if it is empty the separator will not be displayed $quote3

"""

  def quote1 = { q("hello") === "'hello'" }
  def quote2 = { "Warning".prefix(": ", "dangerous") === "Warning: dangerous" }
  def quote3 = { "".prefix(": ", "dangerous")        === "dangerous"          }

}
