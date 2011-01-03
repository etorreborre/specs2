package org.specs2
package text
import Quote._

class QuoteSpec extends SpecificationWithJUnit { def is =
  "A string can be prefixed to another with a separator"                  ! prefix1
  "but if it is empty the separator will not be displayed"                ! prefix2

  def prefix1 = "Warning" prefix(": ", "dangerous") must_== "Warning: dangerous"
  def prefix2 = "" prefix(": ", "dangerous") must_== "dangerous"
}