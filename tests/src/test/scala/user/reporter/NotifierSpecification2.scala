package user
package reporter
import org.specs2._
import specification._
import org.specs2.execute._

class NotifierSpecification2 extends Specification { def is =
      "intro"                                             ^
                                                          p^
      "first group"                                       ^
        "ex1" ! success                                   ^
        "ex2" ! success                                   ^
                                                          p^
      "second group"                                      ^
        "ex3" ! success                                   ^
        "ex4" ! success                                   ^
                                                          end
  }