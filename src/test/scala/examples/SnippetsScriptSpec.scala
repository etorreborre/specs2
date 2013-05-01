package examples

import org.specs2._
import specification.script._

class SnippetsScriptSpec extends Specification with Snippets { def is = s2"""      ${library.start}

 This is some documentation for an API where pieces of code can be inserted

  > snippet 1 <

 And also here

  > snippet 2 <

 This is as simple as that!                                                     ${library.end}
"""

  lazy val s1 = snippet {
    1 +
    1 == 2
  }

  lazy val s2 = snippet {
    2 + 2
  }.eval

  lazy val library =
    SnippetsLibrary("simple").
      add("snippet 1")(s1).
      add("snippet 2")(s2)

}
