package org.specs2
package guide
package matchers

import form.Card

object NumericMatchers extends UserGuideCard {
  def title = "Numeric"
  def text = s2"""
Numerical values can be compared with the following matchers

 * `beLessThanOrEqualTo` compares any `Ordered` type with `<=`
   `1 must be_<=(2)`
   `1 must beLessThanOrEqualTo(2)`

 * `beLessThan` compares any `Ordered` type with `<`
   `1 must be_<(2)`
   `1 must beLessThan(2)`

 * `beGreaterThanOrEqualTo` compares any `Ordered` type with `>=`
   `2 must be_>=(1)`
   `2 must beGreaterThanOrEqualTo(1)`

 * `beGreaterThan` compares any `Ordered` type with `>`
   `2 must be_>(1)`
   `2 must beGreaterThan(1)`

 * `beCloseTo` check if 2 Numerics are close to each other
   `1.0 must beCloseTo(1, 0.5)`
   `4 must be ~(5 +/- 2)`

 * `beBetween` check if a value is between 2 others
   `5 must beBetween(3, 6)`
   `5 must beBetween(3, 6).excludingEnd`
   `5 must beBetween(4, 6).excludingStart`
   `5 must beBetween(4, 6).excludingBounds`
   `// with brackets notation`
   `5 must (be`\``[`\``(4, 7)`\``]`\``)`
                                                                                                                  ."""
}
