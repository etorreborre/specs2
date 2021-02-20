package user.specification

import org.specs2.execute.*

class UserExecutionSpecification extends org.specs2.Specification:
  def is = "ex1" ! Failure("fail")
