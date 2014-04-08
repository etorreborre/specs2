package user
package reporter
import org.specs2._
import matcher.DataTables
import specification._
import org.specs2.execute._

class NotifierSpecification extends Specification with DataTables { def is =
      "intro"                                                         ^p^
      "first group"                                                   ^
        "ex1" ! success                                               ^
        "ex2" !
          Failure("fail")                                             ^
        "ex3" !
          Error("skipped", new Exception("error"))                    ^
        "ex4" ! Skipped("skipped")                                    ^
        "ex5" ! Pending("pending")                                    ^
        "ex6" ! ("a" | "b" |> 1 ! 2 | { (a, b) => success })          ^
        Step(Failure("clean failed"))                                 ^
        Step("clean ok")                                              ^
                                                                      end

}