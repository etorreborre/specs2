package org.specs2
package guide

class QuickStart extends Specification { def is = literate                                                              ^
  "Quick Start".title ^
"#### First specification"                                                                                              ^
"Here is your first specification with ***specs2***:                                                                     "^
                                                                                                                        """
    import org.specs2._

    class HelloWorldSpec extends Specification { def is =

      "This is a specification to check the 'Hello world' string"                 ^
                                                                                  p^
      "'Hello world' contains 11 characters"                                      ! e1^
      "'Hello world' starts with 'Hello'"                                         ! e2^
      "'Hello world' ends with 'world'"                                           ! e3^
                                                                                  end
      def e1 = "Hello world" must have size(11)
      def e2 = "Hello world" must startWith("Hello")
      def e3 = "Hello world" must endWith("World")
    }

The class containing your specification must extend `org.specs2.Specification` and must define a method called `is`.

The `is` method lists *specification fragments* which can be:

* some text: like a description of the system you're specifying
* an example: a description and some executable code returning a result
* formatting fragments: to enhance the display of the specification by adding line breaks or indentation

You can notice also that fragments are separated by the `^` character in order to build a list of them.
                                                                                                                        """^
"#### Execution"                                                                                                        ^
                                                                                                                        """
And this is it! Now to execute your specification, you use a *runner* which will display the results:

    > scala -cp ... specs2.run HelloWorldSpec

    HelloWorldSpec

    This is a specification to check the 'Hello world' string

    + 'Hello world' contains 11 characters
    + 'Hello world' starts with 'Hello'
    + 'Hello world' ends with 'world'

    Total for specification HelloWorldSpec
    Finished in 0 second, 78 ms
    3 examples, 0 failure, 0 error
                                                                                                                        """^
"#### Where to go from here"                                                                                            ^
                                                                                                                        """
You can explore the rest of this [User Guide](org.specs2.UserGuide.html "Guide") to learn how to:

 * display your text and examples nicely
 * define _contexts_ to setup/teardown data for your examples
 * include / link specifications and reuse examples
 * use the many ***specs2*** matchers to specify precise expectations
 * use Mockito or ScalaCheck
 * use sbt/maven/junit to execute a specification
 * export your specification as an html document (like this one!)
                                                                                                                        """^
  include(xonly, new examples.HelloWorldSpec)                                                                                    ^
                                                                                                                        end
}
