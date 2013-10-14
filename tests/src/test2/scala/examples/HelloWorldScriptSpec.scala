package examples

import org.specs2._
import specification._

/**
 * This specification shows how to create examples using the "script" style
 */
class HelloWorldScriptSpec extends script.Specification with Groups { def is = s2"""

 This is a specification to check the 'Hello world' string

 The 'Hello world' string should
   + contain 11 characters
   + start with 'Hello'
   + end with 'world'
                                                                                 """

  "hello world" - new group {
    eg := "Hello world" must have size(11)
    eg := "Hello world" must startWith("Hello")
    eg := "Hello world" must endWith("world")
  }

}
