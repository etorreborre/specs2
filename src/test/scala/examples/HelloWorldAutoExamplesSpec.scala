package examples

import org.specs2._

/**
 * This specification shows that it is possible to use the example code directly in the specification
 */
class HelloWorldAutoExamplesSpec extends Specification { def is = s2"""

  This is a specification to check the 'Hello world' string

  The 'Hello world' string satisfies the following properties:

   ${eg { "Hello world" must have size(11)      }}
   ${eg { "Hello world" must startWith("Hello") }}
   ${eg { "Hello world" must endWith("world")   }}

                                                                  """

}
