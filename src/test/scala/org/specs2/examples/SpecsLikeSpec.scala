package org.specs2
package examples
import specification._

class SpecsLikeSpec extends SpecificationWithJUnit { def is =
                                                                                          """
This specification shows how to write a specification in the specs style.
You need to:
 * replace `should` by `^` and `in` by `!`
 * chain examples with `^`
 * separate blocks of examples with `p^`
                                                                                          """^
                                                                                          p^
  "'Hello world' should" ^ {
    "contain 11 characters" ! {
      "Hello world" must have size(11)
    }^
    "start with 'Hello'" ! {
      "Hello world" must startWith("Hello")
    }^
    "with 'world'" ! {
      "Hello world" must endWith("world")
    }
  }^
  p^
  "'Hey you' should" ^ {
    "contain 7 characters" ! {
      "Hey you" must have size(7)
    }
  }
}
