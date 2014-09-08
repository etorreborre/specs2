package examples

import org.specs2._
import specification._

/**
 * This specification shows how to use the `script.Specification` style where there is no need to interpolate the examples
 * in the specification text but just to mark them with `+`
 */
class ScriptedHelloWorldSpec extends script.Specification with Groups { def is = s2"""

 This is a specification to check the 'Hello world' string

 The 'Hello world' string should
   + contain 11 characters
   + start with 'Hello'
   + end with 'world'
                                                                                   """

  "examples" - new group {
    eg := "Hello world" must haveSize(11)
    eg := "Hello world" must startWith("Hello")
    eg := "Hello world" must endWith("world")
  }

}
