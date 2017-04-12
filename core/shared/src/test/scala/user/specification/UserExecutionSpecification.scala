package user.specification

import org.specs2.execute._

class UserExecutionSpecification extends org.specs2.Specification {
  def is = "ex1" ! Failure("fail")
}
