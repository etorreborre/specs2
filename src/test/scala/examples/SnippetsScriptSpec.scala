package examples

import org.specs2._
import specification._

/**
 * This specification shows that it is possible to insert code snippets in some text
 * to document the API of a system
 */
class SnippetsScriptSpec extends Specification with Snippets { def is = s2"""

 This is some documentation for an API where pieces of code can be inserted. Here:

  $s1

 And also here

  $s2

 This is as simple as that!
"""



  lazy val s1 = snippet {
    1 +
    1 == 2
  }.offsetIs(-4)

  lazy val s2 = snippet {
    2 + 2
  }.offsetIs(-4).eval

}
