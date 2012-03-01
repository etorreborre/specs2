package examples

import org.specs2._
import org.scalacheck._

/**
 * This example shows how to implement the examples shown in the ScalaCheck UserGuide:
 * http://code.google.com/p/scalacheck/wiki/UserGuide
 *
 * Boolean assertions are replaced by `must` expectations to get better failure messages in case of a failure
 *
 * (`===` is an alias for `must_==`)
 */
class ScalaCheckExamplesSpec extends Specification with ScalaCheck { def is =

  "startsWith" ! check { (a: String, b: String) => (a+b) must startWith(a) }                                            ^
  "endsWith"   ! check { (a: String, b: String) => (a+b) must endWith(b) }                                              ^
  "substring"  ! check { (a: String, b: String) => (a+b).substring(a.length) must_== b }                                ^
  "substring"  ! check { (a: String, b: String, c: String) => (a+b+c).substring(a.length, a.length+b.length) === b }    ^
                                                                                                                        end
}