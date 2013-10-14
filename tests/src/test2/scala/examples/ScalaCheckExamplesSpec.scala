package examples

import org.specs2._

/**
 * This specification shows how to implement the examples presented in the ScalaCheck UserGuide:
 * https://github.com/rickynils/scalacheck/wiki/User-Guide
 *
 * Boolean assertions are replaced by `must` expectations to get better failure messages in case of a failure
 *
 */
class ScalaCheckExamplesSpec extends Specification with ScalaCheck { def is = s2"""

  startsWith ${ prop { (a: String, b: String) => (a+b) must startWith(a) } }
  endsWith   ${ prop { (a: String, b: String) => (a+b) must endWith(b) } }
  substring  ${ prop { (a: String, b: String) => (a+b).substring(a.length) === b } }
  substring  ${ prop { (a: String, b: String, c: String) => (a+b+c).substring(a.length, a.length+b.length) === b } }
                                                                                                                        """
}