package user
package reporter
import org.specs2._
import specification._
import org.specs2.execute._

class NotifierSpecification extends Specification { def is =
      "intro"                                             ^p^
      "first group"                                       ^
        "ex1" ! success                                   ^
        "ex2" !
          Failure("fail")                           ^
        "ex3" !
          Error("skipped", new Exception("error"))  ^
        "ex4" ! Skipped("skipped")                        ^
        "ex5" ! Pending("pending")                        ^
        Step(Failure("clean failed"))                     ^
        Step("clean ok")                                  ^
                                                          end
  }