package org.specs2
package text

import Regexes._

class RegexesSpec extends Spec { def is = s2"""
 `matchesSafely` can be used to match with string which might be malformed regular expression
   if the expression is well formed     $e1
   if the expression is not well formed $e2
"""

  def e1 = "eric" matchesSafely ".*r.c"
  def e2 = "][" matchesSafely "]["
}
