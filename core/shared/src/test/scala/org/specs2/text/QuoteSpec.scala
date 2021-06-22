package org.specs2
package text

import Quote._
import matcher._

class QuoteSpec extends Spec with TypedEqual { def is = s2"""

 A string can be added as a prefix to another with a separator $quote1
 but if it is empty the separator will not be displayed $quote2

 An Option must be quoted $quote3

"""

 def quote1 = { "Warning".prefix(": ", "dangerous") === "Warning: dangerous" }
 def quote2 = { "".prefix(": ", "dangerous")        === "dangerous" }
 def quote3 = { q("hello") === "'hello'" }

}
