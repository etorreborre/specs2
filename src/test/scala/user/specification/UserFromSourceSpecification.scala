package user
package specification

import org.specs2.io.WithLocation


class UserFromSourceSpecification extends org.specs2.Specification { def is = noindent^
                                                                                      p^
  "this is a one-liner"                                                               ^
  { 1 must_== 1 }                                                                     ^
  "this is an example on several lines"                                               ^
  { val a = "hello"
    val b = " world"
    (a + b) must_== "hello world"
  }                                                                                   ^
                                                                                      end
}

