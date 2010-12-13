package org.specs2
package guide

class QuickStart extends Specification { def is = args(noindent = true, debugMarkdown=true)^
  "Here is your first specification with specs2:                                              "^
                                                                                              """
      import org.specs2._

      class HelloWorldSpec extends Specification { def is =

        "This is a specification to check the 'Hello world' string"                    ^
        "'Hello world' contains 11 characters"                                         ! e1^
        "'Hello world' starts with 'Hello'"                                            ! e2^
        "'Hello world' ends with 'world'"                                              ! e3^
                                                                                       end
        def e1 = "Hello world" must have size(11)
        def e2 = "Hello world" must startWith("Hello")
        def e3 = "Hello world" must endWith("World")
      }

  The class above extends *org.specs2.Specification* and must define a method called `is`.
  This methods lists specification fragments which are:
  * simple text fragments: 'This is a specification to check the 'Hello world' string'
  * examples: a String followed by ! and a method returning a `Result`

  `Result`s are usually created by defining expectations with `Matchers`:
  * `"Hello world" must startWith("Hello")`                                                         
                                                                                                 """^
                                                                                                 end
  class HelloWorldSpec extends Specification { def is =

    "This is a specification to check the 'Hello world' string"                    ^
    "'Hello world' contains 11 characters"                                         ! e1^
    "'Hello world' starts with 'Hello'"                                            ! e2^
    "'Hello world' ends with 'world'"                                              ! e3^
                                                                                   end
    def e1 = "Hello world" must have size(11)
    def e2 = "Hello world" must startWith("Hello")
    def e3 = "Hello world" must endWith("World")
  }

}